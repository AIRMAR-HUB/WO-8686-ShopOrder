create or replace package AMR_SHOP_ORDER_API is
  module_  CONSTANT VARCHAR2(25) := 'AMR';
  lu_name_ CONSTANT VARCHAR2(25) := 'AmrShopOrderApi';
  lu_type_ CONSTANT VARCHAR2(25) := 'Entity';

  -- Author  : PRJON
  -- Created : 12/18/2019 10:32:27 AM
  -- Purpose : Mods related to Shop order activities

PROCEDURE UNRECEIVE_PART(
                               ORDER_NO_    IN VARCHAR2,
                               REL_         IN VARCHAR2,
                               SEQ_         IN VARCHAR2,
                               CONTRACT_    IN VARCHAR2);
                               
PROCEDURE UNISSUE_PART(
                               ORDER_NO_    IN VARCHAR2,
                               REL_         IN VARCHAR2,
                               SEQ_         IN VARCHAR2,
                               CONTRACT_    IN VARCHAR2);

PROCEDURE UNRECEIVE_UNISSUE_PART(
                               ORDER_NO_    IN VARCHAR2,
                               REL_         IN VARCHAR2,
                               SEQ_         IN VARCHAR2,
                               CONTRACT_    IN VARCHAR2);
end AMR_SHOP_ORDER_API;
/
create or replace package body AMR_SHOP_ORDER_API is

PROCEDURE UNRECEIVE_PART(
                               ORDER_NO_    IN VARCHAR2,
                               REL_         IN VARCHAR2,
                               SEQ_         IN VARCHAR2,
                               CONTRACT_    IN VARCHAR2)
IS

  Info_                             VARCHAR2(32000) := NULL;
  Transaction_Qty_                  NUMBER          := 1;
  Transaction_Catch_Qty_            NUMBER          := NULL;
  Line_Item_No_                     NUMBER          := NULL;
  
  -- TEMP --
/*  
  ORDER_NO_TEMP_                    VARCHAR2(200)  := '83716';
  REL_TEMP_                         VARCHAR2(200)  := '*';
  SEQ_TEMP_                         VARCHAR2(200)  := '*';
  CONTRACT_TEMP_                    VARCHAR2(200)  := 'AMR';
*/
  -- TEMP --
  TEMP_                             VARCHAR2(200)  := '*';
  
  CURSOR LINES_TO_PROCESS IS
  SELECT t.OBJID, t.OBJVERSION, t.QUANTITY, nvl(t.quantity,0) - nvl(t.qty_reversed,0) rev_qty, 
         AIRM1APP.Inventory_Part_API.Get_Catch_Unit_Meas(t.CONTRACT, t.PART_NO), 
         t.PART_NO, t.CONTRACT, t.LOCATION_NO, t.LOT_BATCH_NO, t.SERIAL_NO, t.CONDITION_CODE, 
         AIRM1APP.CONDITION_CODE_API.Get_Description(t.CONDITION_CODE), t.WAIV_DEV_REJ_NO, 
         t.ACTIVITY_SEQ, t.ENG_CHG_LEVEL, t.SOURCE_REF1, t.SOURCE_REF2, t.SOURCE_REF3, t.SOURCE_REF4, 
         AIRM1APP.Inventory_Location_API.Get_Warehouse(t.CONTRACT, t.LOCATION_NO) a, 
         AIRM1APP.Inventory_Location_API.Get_Bay_No(t.CONTRACT, t.LOCATION_NO) b, 
         AIRM1APP.Inventory_Location_API.Get_Row_No(t.CONTRACT, t.LOCATION_NO) c, 
         AIRM1APP.Inventory_Location_API.Get_Tier_No(t.CONTRACT,t.LOCATION_NO) d, 
         AIRM1APP.Inventory_Location_API.Get_Bin_No(t.CONTRACT, t.LOCATION_NO) e, 
         t.TRANSACTION_CODE, t.TRANSACTION, t.TRANSACTION_ID, t.ACCOUNTING_ID, t.PART_OWNERSHIP, 
         t.OWNING_CUSTOMER_NO, AIRM1APP.CUST_ORD_CUSTOMER_API.Get_Name(t.OWNING_CUSTOMER_NO), 
         t.CF$_AMR_RETURN_SCRAP_DESC, t.CF$_LOCATION 
  FROM AIRM1APP.INVENTORY_TRANSACTION_HIST_CFV t
   WHERE nvl(t.quantity,0) - nvl(t.qty_reversed,0) > 0 
    AND t.direction = '+' AND t.transaction_code = 'OOREC' 
    AND (AIRM1APP.Shop_Ord_Code_API.Encode(AIRM1APP.Shop_Ord_API.Get_Order_Code(source_ref1, source_ref2, source_ref3)) <> 'B' OR source_ref4 <> -1) 
    AND t.SOURCE_REF1 = ORDER_NO_ AND t.SOURCE_REF2 = REL_ 
    AND t.SOURCE_REF3 = SEQ_      AND t.CONTRACT    = CONTRACT_
  ORDER BY t.part_no, t.date_created;
    
BEGIN
  FOR REC_ IN LINES_TO_PROCESS LOOP
    EXIT WHEN LINES_TO_PROCESS%NOTFOUND;
    
    SELECT line_item_no INTO Line_Item_No_
       FROM manufactured_part_tab 
         WHERE order_no = REC_.source_ref1 AND part_no = REC_.part_no AND contract = REC_.contract;

   
    TEMP_ := REC_.source_ref1||' UR-'||REC_.part_no||'-'||REC_.quantity||'-'||REC_.rev_qty;
    INSERT INTO A_TEMP_TAB VALUES(TEMP_, 'PROC', SYSDATE); 
      
    -- Transaction_Qty_ := REC_.rev_qty;
    IF Transaction_Qty_ > 0 THEN
      
     AIRM1APP.Shop_Ord_API.Unreceive_Part__(Info_, REC_.source_ref1, REC_.source_ref2, REC_.source_ref3, 
       REC_.contract, REC_.part_no, REC_.location_no, REC_.lot_batch_no, REC_.serial_no, REC_.eng_chg_level,
       REC_.waiv_dev_rej_no, Transaction_Qty_,
       REC_.quantity, REC_.accounting_id, REC_.transaction_id, Transaction_Catch_Qty_, Line_Item_No_,
       REC_.condition_code);
     
    END IF; 
   
  END LOOP;
  
  COMMIT;  
  DBMS_OUTPUT.PUT_LINE('UNREECEIVE - Completed');

  EXCEPTION 
    WHEN OTHERS THEN
      ROLLBACK;
      DBMS_OUTPUT.PUT_LINE('ERROR(unreceive): '||SQLCODE||' - '||SUBSTR(SQLERRM, 1, 300));
END;



PROCEDURE UNISSUE_PART(
                               ORDER_NO_    IN VARCHAR2,
                               REL_         IN VARCHAR2,
                               SEQ_         IN VARCHAR2,
                               CONTRACT_    IN VARCHAR2)
IS

  Info_                             VARCHAR2(32000) := NULL;
  Transaction_Qty_                  NUMBER          := 1;
  Transaction_Catch_Qty_            NUMBER          := NULL;
  Line_Item_No_                     NUMBER          := NULL;

  
  -- TEMP --
/*  
  ORDER_NO_TEMP_                    VARCHAR2(200)  := '83716';
  REL_TEMP_                         VARCHAR2(200)  := '*';
  SEQ_TEMP_                         VARCHAR2(200)  := '*';
  CONTRACT_TEMP_                    VARCHAR2(200)  := 'AMR';
*/
  -- TEMP --
  TEMP_                             VARCHAR2(2000)  := '*';
  
CURSOR LINES_TO_PROCESS IS
  SELECT t.*, nvl(t.quantity,0) - nvl(t.qty_reversed,0) rev_qty
   FROM  AIRM1APP.INVENTORY_TRANSACTION_HIST t
    WHERE  SOURCE_REF1 = ORDER_NO_ 
       AND SOURCE_REF2 = REL_      
       AND SOURCE_REF3 = SEQ_      
       AND CONTRACT    = CONTRACT_
       AND direction   = '-'
       AND TRANSACTION_CODE IN ('SOISS', 'BACFLUSH', 'CO-SOISS', 'CO-BACFLSH');  
       
BEGIN
  FOR REC_ IN LINES_TO_PROCESS LOOP
    EXIT WHEN LINES_TO_PROCESS%NOTFOUND;

     TEMP_ := REC_.source_ref1||' UI~'||REC_.part_no||'~'||REC_.quantity||'~'||REC_.rev_qty;
     INSERT INTO A_TEMP_TAB VALUES(TEMP_, 'PROC', SYSDATE);
     DBMS_OUTPUT.PUT_LINE(TEMP_);
     
      IF  (REC_.rev_qty) < 1 THEN
        Transaction_Qty_ := 0;
      ELSE
        Transaction_Qty_ := 1;
      END IF;
      

    --Transaction_Qty_ := REC_.rev_qty;
    
    IF Transaction_Qty_ > 0 THEN
      
     AIRM1APP.Shop_Material_Alloc_API.Unissue_Part(Info_, REC_.source_ref1, REC_.source_ref2, REC_.source_ref3, 
       REC_.source_ref4, REC_.contract, REC_.part_no, REC_.location_no, REC_.lot_batch_no, REC_.serial_no, 
       REC_.eng_chg_level, REC_.waiv_dev_rej_no, Transaction_Qty_,
       REC_.quantity, REC_.qty_reversed, REC_.transaction_id, REC_.accounting_id,  REC_.activity_seq,
       Transaction_Catch_Qty_);
      
     END IF;  
      
  END LOOP;
  
   COMMIT;   
 DBMS_OUTPUT.PUT_LINE('UNISSUE - Completed');
  EXCEPTION 
    WHEN OTHERS THEN
      ROLLBACK;
      DBMS_OUTPUT.PUT_LINE('ERROR(unissue): '||SQLCODE||' - '||SUBSTR(SQLERRM, 1, 300));
END;


PROCEDURE UNRECEIVE_UNISSUE_PART(
                               ORDER_NO_    IN VARCHAR2,
                               REL_         IN VARCHAR2,
                               SEQ_         IN VARCHAR2,
                               CONTRACT_    IN VARCHAR2)
IS

  Info_                             VARCHAR2(32000) := NULL;
  Transaction_Qty_                  NUMBER          := 1;
  Transaction_Catch_Qty_            NUMBER          := NULL;
  Line_Item_No_                     NUMBER          := NULL;
   
  
  -- TEMP --
/*  
  ORDER_NO_TEMP_                    VARCHAR2(200)  := '83716';
  REL_TEMP_                         VARCHAR2(200)  := '*';
  SEQ_TEMP_                         VARCHAR2(200)  := '*';
  CONTRACT_TEMP_                    VARCHAR2(200)  := 'AMR';
*/ 
  TEMP_                             VARCHAR2(200)  := '*';
  -- TEMP --
     
BEGIN
  TEMP_ := ORDER_NO_||'-'||REL_||'-'||SEQ_;
  INSERT INTO A_TEMP_TAB VALUES(TEMP_, 'PROC', SYSDATE);    COMMIT;
  
  UNRECEIVE_PART(ORDER_NO_, REL_, SEQ_, CONTRACT_);
  
  UNISSUE_PART(ORDER_NO_, REL_, SEQ_, CONTRACT_);

  COMMIT;   
  DBMS_OUTPUT.PUT_LINE('COMBO - Completed');
  EXCEPTION 
    WHEN OTHERS THEN
      ROLLBACK;
      DBMS_OUTPUT.PUT_LINE('ERROR(unissue): '||SQLCODE||' - '||SUBSTR(SQLERRM, 1, 300));
END;



end AMR_SHOP_ORDER_API;
/
