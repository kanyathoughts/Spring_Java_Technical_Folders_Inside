CREATE OR REPLACE FUNCTION jsonb_merge(in_t jsonb, in_u jsonb) RETURNS jsonb
	LANGUAGE sql IMMUTABLE PARALLEL SAFE
AS $BODY$
SELECT jsonb_strip_nulls(
	jsonb_object_agg(
		coalesce(t.key, u.key),
		CASE
			WHEN t.val isnull THEN u.val
			WHEN u.val isnull THEN t.val
			WHEN jsonb_typeof(t.val) <> 'object' OR jsonb_typeof(u.val) <> 'object' THEN u.val
			ELSE jsonb_merge(t.val, u.val)
		END))
FROM jsonb_each(in_t) t(key, val)
FULL JOIN jsonb_each(in_u) u(key, val) ON t.key = u.key
$BODY$;
