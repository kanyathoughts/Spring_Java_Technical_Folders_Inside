CREATE TABLE "job_info" (
	"id" uuid NOT NULL,
	"name" text NOT NULL,
	"description" text,
	"step_description" text,
	"status" text,
	"pending_tasks" int,
	"total_work_units" int,
	"processed_work_units" float8,
	"submit_time" timestamp_zoned_milli,
	"scheduled_start_time" timestamp_zoned_milli,
	"start_time" timestamp_zoned_milli,
	"finish_time" timestamp_zoned_milli,
	"created_by" text NOT NULL,
	PRIMARY KEY ("id")
);

CREATE TABLE "job_info_messages" (
	"id" uuid REFERENCES "job_info" ON DELETE CASCADE,
	"ordinal" int NOT NULL,
	"text" text NOT NULL,
	"severity" text NOT NULL,
	PRIMARY KEY (id, ordinal)
);

CREATE TABLE "job_info_results" (
	"id" uuid REFERENCES "job_info" ON DELETE CASCADE,
	"name" text NOT NULL,
	"type" text NOT NULL,
	"class" text NOT NULL,
	"content" bytea NOT NULL,
	PRIMARY KEY (id, name)
);
