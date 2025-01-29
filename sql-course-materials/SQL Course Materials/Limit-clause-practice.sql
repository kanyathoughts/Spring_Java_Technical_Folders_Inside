select * 
from customers
#limit 3;
		-- if limit exceeds the existing records count in the table then it will not throw any error instead it will return all the records
-- limit 300;

-- page 1 : 1-3
-- page 2 : 4-6
-- page 3 : 7-9
		 -- This means it will skip the first 6 records and return the next 3 records
limit 6, 3;

-- execersize
		-- return the first 3 customers who has more points
select * 
from customers
order by points desc
limit 3;