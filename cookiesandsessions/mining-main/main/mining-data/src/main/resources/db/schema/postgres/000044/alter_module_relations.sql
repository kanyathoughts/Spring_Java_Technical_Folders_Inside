-- WMIN-13398, this migration script was supposed to be reverted, but as it was included in the 04/18 release we can no longer revert it
-- the additional enum value should not cause any harm. Commenting out the contents of the migration so new installations will not get the
-- unnecessary enum value any more.
--
-- Alter TYPE "module_relationship_type"
--  	ADD VALUE 'ARTIFICIAL';
