CREATE OR REPLACE FUNCTION public.functional_block_link_condition_delete_unreferenced()
    RETURNS trigger
    LANGUAGE 'plpgsql'
AS $BODY$
   BEGIN
     WITH unreferenced_conditions AS (
		SELECT cond.uid
		FROM deleted_links d
		JOIN functional_block_link_condition cond ON d.condition = cond.uid
		LEFT JOIN functional_block_link l ON cond.uid = l.condition
		WHERE l.uid IS NULL
	) DELETE FROM functional_block_link_condition WHERE uid IN (SELECT * FROM unreferenced_conditions);
	 RETURN NULL;
   END;
$BODY$;

CREATE OR REPLACE TRIGGER "functional_block_link_delete_condition"
    AFTER DELETE ON "functional_block_link"
    REFERENCING OLD TABLE AS deleted_links
    FOR EACH STATEMENT
    EXECUTE FUNCTION functional_block_link_condition_delete_unreferenced();

CREATE OR REPLACE TRIGGER "functional_block_link_update_condition"
    AFTER UPDATE ON "functional_block_link"
    REFERENCING OLD TABLE AS deleted_links
    FOR EACH STATEMENT
    EXECUTE FUNCTION functional_block_link_condition_delete_unreferenced();
