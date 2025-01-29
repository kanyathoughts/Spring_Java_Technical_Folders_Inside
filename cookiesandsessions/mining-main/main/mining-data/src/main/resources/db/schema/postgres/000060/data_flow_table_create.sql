-- Replaces ProxyContainer
CREATE TABLE proxy_container (
   "id" uuid NOT NULL,
   "data_flow_id" text NOT NULL,
   "module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
   "statement" uuid REFERENCES "ast_node" ON DELETE CASCADE,
   "type" text NOT NULL,
   "properties" jsonb NOT NULL,
    PRIMARY KEY ("id")
);
CREATE INDEX "proxy_container_module_idx" ON "proxy_container" USING btree ("module");
CREATE INDEX "proxy_container_ast_node_idx" ON "proxy_container" USING btree ("statement");
CREATE INDEX "proxy_container_data_flow_id" ON "proxy_container" USING btree ("data_flow_id");

-- Replaces DataFlowNode
CREATE TABLE data_flow_node (
    "id" uuid NOT NULL,
    "data_flow_id" text NOT NULL,
    "module" uuid NOT NULL REFERENCES "module" ON DELETE CASCADE,
    "ast_node" uuid REFERENCES "ast_node" ON DELETE CASCADE,
    "proxy_container" uuid REFERENCES "proxy_container" ON DELETE CASCADE,
    "name" text NOT NULL,
    "traced" bool NOT NULL,
    "type" text NOT NULL,
    PRIMARY KEY ("id")
);
CREATE INDEX "data_flow_node_module_idx" ON "data_flow_node" USING btree ("module");
CREATE INDEX "data_flow_node_ast_node_idx" ON "data_flow_node" USING btree ("ast_node");
CREATE INDEX "data_flow_node_data_flow_id" ON "data_flow_node" USING btree ("data_flow_id");

-- Replaces DataFlowNode.errors
CREATE TABLE "data_flow_error" (
    "data_flow_node" uuid NOT NULL REFERENCES "data_flow_node" ON DELETE CASCADE,
    "severity" text NOT NULL,
    "text" text NOT NULL
);
CREATE INDEX "data_flow_error_node_idx" ON "data_flow_error" USING btree ("data_flow_node");

--add the Enums for Severity, Proxy- and Node Type
CREATE TYPE "data_flow_node_relationship_type" AS ENUM (
        'PROXY_SHORTCUT',
        'RELATED_FIELD',
        'WRITE_ACCESS',
        'READ_ACCESS'
);

-- Replaces DataFlowNode properties: proxyShortcuts, readAccesses, relatedFields, writeAccesses
CREATE TABLE data_flow_node_relationship (
    "src" uuid NOT NULL REFERENCES "data_flow_node" ON DELETE CASCADE,
    "dst" uuid NOT NULL REFERENCES "data_flow_node" ON DELETE CASCADE,
    "type" data_flow_node_relationship_type NOT NULL,
    PRIMARY KEY("src", "dst", "type")
);

CREATE INDEX data_flow_node_relationship_dst_idx on data_flow_node_relationship (dst);

-- Replaces DataFlowNode.fields
CREATE TABLE proxy_container_field (
    "proxy_container" uuid NOT NULL REFERENCES "proxy_container" ON DELETE CASCADE,
    "data_flow_node" uuid NOT NULL REFERENCES "data_flow_node" ON DELETE CASCADE,
    "ordinal" int NOT NULL,
    PRIMARY KEY (proxy_container, data_flow_node, ordinal)
);

CREATE INDEX proxy_container_field_data_flow_node_idx on proxy_container_field (data_flow_node);
