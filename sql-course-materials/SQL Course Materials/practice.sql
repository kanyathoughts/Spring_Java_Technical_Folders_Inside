use sql_store;
# select DISTINCT state from customers;
/*select 
	first_name, 
    last_name, 
    points,
    (points + 10) * 100 as 'discount factor'
from customers; */
-- from customers 
-- WHERE customer_id=1
-- order by first_name;

select 
	name, 
    unit_price,
    unit_price * 1.1 as `new price`
from products;
    