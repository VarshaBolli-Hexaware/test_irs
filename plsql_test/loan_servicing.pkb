CREATE OR REPLACE PACKAGE BODY loan_servicing_pkg AS

    FUNCTION get_loan_details( 
        p_loan_id IN VARCHAR2 
    ) RETURN loan_processing_pkg.r_loan_details IS 
        v_loan_details loan_processing_pkg.r_loan_details; 
    BEGIN 
        v_loan_details := loan_processing_pkg.get_loan_details(p_loan_id); 
        RETURN v_loan_details; 
    EXCEPTION 
        WHEN OTHERS THEN 
            investor_reporting_pkg.log_exception( 
                NULL, 
                p_loan_id, 
                'LOAN_DETAILS', 
                'HIGH', 
                'Error retrieving loan details: ' SQLERRM 
            ); 
            RAISE; 
    END get_loan_details;

    FUNCTION calculate_late_fees( 
        p_loan_id IN VARCHAR2, 
        p_payment_date IN DATE 
    ) RETURN NUMBER IS 
        v_late_fee NUMBER := 0; 
        v_days_late NUMBER; 
        v_loan_details loan_processing_pkg.r_loan_details; 
    BEGIN 
        v_loan_details := get_loan_details(p_loan_id); 
         
        -- Calculate days late 
        v_days_late := delinquency_processing_pkg.get_days_past_due( 
            p_loan_id, p_payment_date 
        ); 
         
        -- Apply late fee if beyond grace period 
        IF v_days_late > c_grace_period_days THEN 
            v_late_fee := LEAST( 
                v_loan_details.current_balance * c_max_late_fee_rate, 
                100  -- Maximum late fee cap 
            ); 
        END IF; 
         
        RETURN v_late_fee; 
    END calculate_late_fees;

    PROCEDURE update_loan_status( 
        p_loan_id IN VARCHAR2, 
        p_new_status IN VARCHAR2, 
        p_reason_code IN VARCHAR2, 
        p_comments IN VARCHAR2 DEFAULT NULL 
    ) IS 
        v_old_status VARCHAR2(20); 
        v_risk_metrics loan_risk_assessment_pkg.r_risk_metrics; 
    BEGIN 
        -- Get current status 
        SELECT status 
        INTO v_old_status 
        FROM loan_master 
        WHERE loan_id = p_loan_id; 
         
        -- Only proceed if status is actually changing 
        IF v_old_status != p_new_status THEN 
            -- Use MERGE to upsert status change into loan_status_history
            MERGE INTO loan_status_history tgt
            USING (
                SELECT 
                    p_loan_id AS loan_id,
                    v_old_status AS old_status,
                    p_new_status AS new_status,
                    SYSDATE AS change_date,
                    p_reason_code AS reason_code,
                    p_comments AS comments,
                    USER AS created_by
                FROM dual
            ) src
            ON (tgt.loan_id = src.loan_id AND tgt.change_date = src.change_date)
            WHEN MATCHED THEN
                UPDATE SET
                    tgt.old_status = src.old_status,
                    tgt.new_status = src.new_status,
                    tgt.reason_code = src.reason_code,
                    tgt.comments = src.comments,
                    tgt.created_by = src.created_by
            WHEN NOT MATCHED THEN
                INSERT (
                    loan_id, old_status, new_status, change_date, reason_code, comments, created_by
                ) VALUES (
                    src.loan_id, src.old_status, src.new_status, src.change_date, src.reason_code, src.comments, src.created_by
                );

            -- Update loan status 
            UPDATE loan_master 
            SET status = p_new_status, 
                last_status_change = SYSDATE 
            WHERE loan_id = p_loan_id; 
             
            -- Get risk metrics for the loan 
            SELECT * 
            INTO v_risk_metrics 
            FROM TABLE(loan_risk_assessment_pkg.calculate_loan_risk_metrics(p_loan_id)); 
             
            -- If status change indicates increased risk, notify risk management 
            IF p_new_status IN ('DELINQUENT', 'DEFAULT') AND  
               v_risk_metrics.risk_score > 70 THEN 
                investor_reporting_pkg.log_exception( 
                    NULL, 
                    p_loan_id, 
                    'STATUS_CHANGE', 
                    'HIGH', 
                    'High risk loan status changed to ' p_new_status 
                ); 
            END IF; 
             
            -- Update pool metrics 
            FOR r_pool IN ( 
                SELECT pool_id  
                FROM pool_loan_xref  
                WHERE loan_id = p_loan_id 
            ) LOOP 
                pool_performance_pkg.generate_performance_report(r_pool.pool_id); 
            END LOOP; 
        END IF; 
         
        COMMIT; 
    EXCEPTION 
        WHEN OTHERS THEN 
            ROLLBACK; 
            investor_reporting_pkg.log_exception( 
                NULL, 
                p_loan_id, 
                'STATUS_UPDATE', 
                'HIGH', 
                'Error updating loan status: ' SQLERRM 
            ); 
            RAISE; 
    END update_loan_status;

END loan_servicing_pkg;
/
