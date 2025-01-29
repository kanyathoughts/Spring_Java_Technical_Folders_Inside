-- additionally supported TYPES TSQ, TDQ and Queue

UPDATE TypeEnum set name="TSQ" UPSERT where name="TSQ";
UPDATE TypeEnum set name="TDQ" UPSERT where name="TDQ";
UPDATE StorageEnum set name="QUEUE" UPSERT where name="QUEUE";
