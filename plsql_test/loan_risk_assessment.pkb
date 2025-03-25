CREATE OR REPLACE PACKAGE BODY loan_risk_assessment_pkg AS

    FUNCTION calculate_loan_risk_metrics(
        p_loan_id IN VARCHAR2
    ) RETURN r_risk_metrics IS
        v_metrics r_risk_metrics;
        v_loan_details loan_processing_pkg.r_loan_details;
        v_property_value NUMBER;
        v_monthly_income NUMBER;
        v_monthly_debt NUMBER;
    BEGIN
        -- Get loan details
        v_loan_details := loan_processing_pkg.get_loan_details(p_loan_id);
        
        -- Get additional loan data from borrower_info table (assumed to exist)
        SELECT property_value, monthly_income, monthly_debt, credit_score
        INTO v_property_value, v_monthly_income, v_monthly_debt, v_metrics.credit_score
        FROM borrower_info
        WHERE loan_id = p_loan_id;
        
        -- Calculate LTV ratio
        v_metrics.ltv_ratio := v_loan_details.current_balance / v_property_value;
        
        -- Calculate DTI ratio
        v_metrics.dti_ratio := v_monthly_debt / NULLIF(v_monthly_income, 0);
        
        -- Calculate risk score (0-100, higher is riskier)
        v_metrics.risk_score := 
            CASE 
                WHEN v_metrics.credit_score < c_min_credit_score THEN 100
                ELSE (
                    (GREATEST(v_metrics.ltv_ratio - c_high_risk_ltv, 0) * 30) +
                    (GREATEST(v_metrics.dti_ratio - c_high_risk_dti, 0) * 40) +
                    ((720 - LEAST(v_metrics.credit_score, 720)) / 720 * 30)
                )
            END;
            
        -- Determine risk category
        v_metrics.risk_category :=
            CASE
                WHEN v_metrics.risk_score >= 80 THEN 'HIGH_RISK'
                WHEN v_metrics.risk_score >= 50 THEN 'MEDIUM_RISK'
                ELSE 'LOW_RISK'
            END;
            
        v_metrics.loan_id := p_loan_id;
        
        RETURN v_metrics;
    EXCEPTION
        WHEN OTHERS THEN
            investor_reporting_pkg.log_exception(
                NULL,
                p_loan_id,
                'RISK_METRICS_CALCULATION',
                'HIGH',
                'Error calculating risk metrics: ' || SQLERRM
            );
            RAISE;
    END calculate_loan_risk_metrics;
    
    PROCEDURE update_pool_risk_profile(
        p_pool_id IN VARCHAR2
    ) IS
        v_high_risk_count NUMBER := 0;
        v_total_loans NUMBER := 0;
        v_avg_risk_score NUMBER := 0;
        v_risk_metrics r_risk_metrics;
    BEGIN
        -- Process each loan in the pool
        FOR r_loan IN (
            SELECT l.loan_id
            FROM loan_master l
            JOIN pool_loan_xref x ON l.loan_id = x.loan_id
            WHERE x.pool_id = p_pool_id
            AND x.active_flag = 'Y'
        ) LOOP
            v_risk_metrics := calculate_loan_risk_metrics(r_loan.loan_id);
            
            v_avg_risk_score := v_avg_risk_score + v_risk_metrics.risk_score;
            IF v_risk_metrics.risk_category = 'HIGH_RISK' THEN
                v_high_risk_count := v_high_risk_count + 1;
            END IF;
            v_total_loans := v_total_loans + 1;
        END LOOP;
        
        IF v_total_loans > 0 THEN
            v_avg_risk_score := v_avg_risk_score / v_total_loans;
        END IF;
        
        -- Update pool risk metrics
        UPDATE pool_risk_metrics
        SET avg_risk_score = v_avg_risk_score,
            high_risk_loan_count = v_high_risk_count,
            high_risk_loan_pct = ROUND(v_high_risk_count / NULLIF(v_total_loans, 0) * 100, 2),
            last_assessment_date = SYSDATE,
            modified_by = USER
        WHERE pool_id = p_pool_id;
        
        -- Call pool valuation package to update pricing
        pool_valuation_pkg.recalculate_pool_value(p_pool_id);
        
        COMMIT;
    EXCEPTION
        WHEN OTHERS THEN
            ROLLBACK;
            investor_reporting_pkg.log_exception(
                p_pool_id,
                NULL,
                'POOL_RISK_UPDATE',
                'HIGH',
                'Error updating pool risk profile: ' || SQLERRM
            );
            RAISE;
    END update_pool_risk_profile;
    
    FUNCTION get_risk_adjusted_rate(
        p_base_rate IN NUMBER,
        p_risk_metrics IN r_risk_metrics
    ) RETURN NUMBER IS
        v_adjusted_rate NUMBER;
        v_risk_premium NUMBER;
    BEGIN
        -- Calculate risk premium based on risk metrics
        v_risk_premium :=
            CASE p_risk_metrics.risk_category
                WHEN 'HIGH_RISK' THEN 0.02  -- 200 bps
                WHEN 'MEDIUM_RISK' THEN 0.01  -- 100 bps
                ELSE 0
            END;
            
        -- Add additional premium for high LTV
        IF p_risk_metrics.ltv_ratio > c_high_risk_ltv THEN
            v_risk_premium := v_risk_premium + 
                (p_risk_metrics.ltv_ratio - c_high_risk_ltv) * 0.05;
        END IF;
        
        -- Add additional premium for high DTI
        IF p_risk_metrics.dti_ratio > c_high_risk_dti THEN
            v_risk_premium := v_risk_premium + 
                (p_risk_metrics.dti_ratio - c_high_risk_dti) * 0.05;
        END IF;
        
        v_adjusted_rate := p_base_rate + v_risk_premium;
        
        RETURN ROUND(v_adjusted_rate, 4);
    END get_risk_adjusted_rate;
    
END loan_risk_assessment_pkg;
/ 