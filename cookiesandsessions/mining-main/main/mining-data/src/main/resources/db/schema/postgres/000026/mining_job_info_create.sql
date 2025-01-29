CREATE TABLE "mining_job_info" (
	"project" uuid REFERENCES "project" ON DELETE CASCADE,
	"module" uuid REFERENCES "module" ON DELETE CASCADE,
	"job_id" uuid NOT NULL,
	PRIMARY KEY (job_id)
);

CREATE INDEX "mining_job_info_project" ON "mining_job_info" USING btree ("project");
CREATE INDEX "mining_job_info_module" ON "mining_job_info" USING btree ("module");
