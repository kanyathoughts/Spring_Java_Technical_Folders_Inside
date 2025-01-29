use sql_store;

/* select *
from orders o
join customers c
	on o.customer_id = c.customer_id; */

-- This is implicit join condition here without mentioning join condition also it's working but we have to specify where condition
-- if we miss where condition then it will become cross join and each and every record will combine with all the records in other table    
select *
from orders o, customers c
where o.customer_id = c.customer_id;