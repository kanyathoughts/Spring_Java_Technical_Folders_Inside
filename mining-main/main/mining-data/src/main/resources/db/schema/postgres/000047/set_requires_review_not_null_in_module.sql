UPDATE module SET requires_review = false WHERE requires_review IS NULL;
ALTER TABLE module ALTER COLUMN requires_review SET NOT NULL;