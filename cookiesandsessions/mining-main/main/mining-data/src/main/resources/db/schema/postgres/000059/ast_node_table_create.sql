CREATE TABLE ast_node (
    "id" uuid NOT NULL,
    "module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
    "location" ast_node_location NOT NULL,
    "parent" uuid,
    "included_module" uuid REFERENCES "module" ON DELETE SET NULL,
    "sibling" int,
    "type" text NOT NULL,
    "super_types" text[],
    "label" text NOT NULL,
    "properties" jsonb,
    PRIMARY KEY (id)
);

CREATE TYPE "ast_relationship_type" AS ENUM (
        'BINDING',
        'REFERS',
        'REDEFINES',
        'FLOW'
);

-- replaces AstBinding, RefersTo & Redefines edges: AstNode ===> AstNode
-- replaces FlowsControl edges: AstNode ===> AstNode
CREATE TABLE ast_relationship (
    "id" uuid NOT NULL,
    "src" uuid NOT NULL REFERENCES "ast_node" ON DELETE CASCADE,
    "dst" uuid NOT NULL REFERENCES "ast_node" ON DELETE CASCADE,
    "type" ast_relationship_type NOT NULL,
    "label" text NULL CONSTRAINT "ast_relationship_binding_label_not_null_const" CHECK (type != 'BINDING' OR label IS NOT NULL),
    PRIMARY KEY("id")
);
CREATE INDEX "ast_relationship_src_idx" ON "ast_relationship" USING btree ("src");
CREATE INDEX "ast_relationship_dst_idx" ON "ast_relationship" USING btree ("dst");

-- replaces link between Module to EntryPoint, HaltPoint & ReturnPoint to AstNode
CREATE TYPE "ast_module_relationship_type" AS ENUM (
    'ENTRY',
    'HALT',
    'RETURN',
    'ROOT'
);

CREATE TABLE ast_module_relationship (
    "node" uuid NOT NULL REFERENCES "ast_node" ON DELETE CASCADE,
    "type" ast_module_relationship_type NOT NULL,
    "module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
    PRIMARY KEY ("node", "type", "module")
);
CREATE INDEX "ast_module_relationship_module_idx" ON "ast_module_relationship" USING btree ("module");
