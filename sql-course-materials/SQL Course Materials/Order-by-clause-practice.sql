select * from customers
-- order by first_name;
-- order by first_name desc;

		-- table will be sorted in ascending order based on state and if we have same state then in that sorting will be happen in ascending order of first_name
-- order by state, first_name;
		-- table will be sorted in descending order based on state and if we have same state then in that sorting will be happen in descending order of first_name
order by state desc, first_name desc;

		-- here order by 1, 2 means sorting will be happened for first_name and then last_name those are the columns we mentioned in the select statement
        -- and it will consider that order
        -- but this is not recommended way as we can add more columns in select statements then columns order will be changed.
select first_name, last_name, 10 as points
from customers
order by 1, 2; 

-- execercize
-- need to get the order items where order id is 2 and total price in decsnding order
select *, (quantity * unit_price) as total_price
from order_items
where order_id = 2
order by total_price desc



