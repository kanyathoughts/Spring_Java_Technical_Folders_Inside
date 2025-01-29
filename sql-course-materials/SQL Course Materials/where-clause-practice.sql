use `sql_store`;
/* select * 
from customers 
-- where points = 3000 &&  state != 'va';
where birth_date < '1990-01-01'; */

/* select *
from customers
-- This below command first will take and command as it has higher precedence then execute or command
-- we can mention mention same thing like where birth_date > '1990-01-01' or (points > 3000 and state='va');
where birth_date > '1990-01-01' or (points > 3000 and state='va');
-- not operator is used to get negation of the results sets
-- where not (order_date >= '2018-01-01' && order_date <= '2018-12-31'); */


-- execersize
select * from order_items
where order_id=6 && (quantity * unit_price > 30);
