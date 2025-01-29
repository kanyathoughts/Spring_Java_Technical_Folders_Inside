--ALTER TABLE module_relationship ADD COLUMN from_dead_code boolean;
/*
We have reverted the code related to from_dead_code column and added it as a property in properties column.
We are keeping this migration script as it is already merged to main and available in nightly and commenting the
query to avoid any conflicts and creation of column.
*/
