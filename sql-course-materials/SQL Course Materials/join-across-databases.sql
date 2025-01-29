use sql_store;
		-- here we are using sql_store database so if we want to join tables from other database/schema we have to prefix table with that database name
select * 
from order_items oi
join sql_inventory.products p
on oi.product_id=p.product_id;