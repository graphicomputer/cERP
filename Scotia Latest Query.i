FOR EACH pp_perview WHERE 
    pp_perview.kco = {kco} AND 
    (pp_perview.ppw_view = "VISITOR"), 
        EACH pp_person of pp_perview, 
            EACH hs_purchaser WHERE 
                hs_purchaser.kco = pp_perview.kco AND 
                (hs_purchaser.hev_type = 'VISITOR') AND 
                hs_purchaser.ppp_intref = pp_perview.ppp_intref, 
                    EACH hs_visitor WHERE 
                        hs_visitor.kco = pp_perview.kco AND 
                        hs_visitor.hvi_ref = INTEGER(hs_purchaser.hsp_key1) AND 
                        (hs_visitor.hvi_date >= '{RS_hvi_date__1}' OR 
                        '{RS_hvi_date__1}' = '') AND 
                        (hs_visitor.hvi_date <= '{RS_hvi_date__2}' OR 
                        '{RS_hvi_date__2}' = '') AND 
                        CAN-DO('{RS_hvi_status}',hs_visitor.hvi_status), 
                LAST hs_trans OUTER-JOIN WHERE 
                    hs_trans.kco = hs_visitor.kco AND 
                    hs_trans.hev_type = "VISITOR" AND 
                    hs_trans.htr_key1 = STRING(hs_visitor.hvi_ref) AND 
                    hs_trans.htr_key2 = "" AND 
                    hs_trans.htr_key3 = "" AND 
                    hs_trans.hca_ref  = 0  AND 
                    hs_trans.htr_date >= hs_visitor.hvi_date AND 
                    hs_trans.htr_delete = NO AND 
                    hs_trans.hev_code = 'PURCHASE', 
        LAST hs_event OUTER-JOIN OF hs_trans, 
        LAST hs_action OUTER-JOIN WHERE 
            hs_action.kco = hs_visitor.kco AND 
            hs_action.hsa_key1 = STRING(hs_visitor.hvi_ref)
            
            
            
            
            
            
vp_site.RO_vsi_VisitorMovement^VISIT|WK|0||{RS_hcl_caldate__2}
RO_vsi_VisitorMovement
^<Event> |<PeriodType>[ |<PeriodOffset>[ |<FromDate>[ |<ToDate>[ |<FromDays>[ |<ToDays>[ |<LevInt>]]]]]]




vp_site.RO_vsi_VisitorCount^YR|0||{RS_hcl_caldate__2}
    RO_vsi_VisitorCount
^|<FromDate>[ |<ToDate>[ |<FromDays>[ |<ToDays>]]]]]
        
        
        
FOR EACH gl_config WHERE 
        can-do('1,2,3',STRING(gl_config.kco)), 
EACH gl_acct WHERE gl_acct.kco = gl_config.kco {glaSelect}