CREATE TYPE ast_node_location AS (
	retraced_offset integer,
	retraced_length integer,
	assembled_offset integer,
	assembled_length integer,
	root_relative_offset integer,
	root_relative_length integer,
	root_relative_start_line integer,
	root_relative_end_line integer
);
