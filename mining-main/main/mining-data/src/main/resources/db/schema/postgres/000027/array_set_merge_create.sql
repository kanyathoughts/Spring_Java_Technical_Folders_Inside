CREATE OR REPLACE FUNCTION array_set_merge(in_base anyarray, in_add anyarray, in_rem anyarray) RETURNS anyarray
	LANGUAGE sql IMMUTABLE PARALLEL SAFE 
AS $BODY$
SELECT array(
	SELECT DISTINCT v FROM unnest(in_base || in_add) t(v)
	WHERE v IS NOT null AND (in_rem IS null OR NOT v = any(in_rem))
	ORDER BY v
);
$BODY$;
