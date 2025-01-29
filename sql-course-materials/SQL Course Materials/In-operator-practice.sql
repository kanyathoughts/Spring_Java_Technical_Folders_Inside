use `sql_store`;

select * from customers
-- in is used to specify we have to get the customers which are present in mentioned states it's more like or operator
-- where state = 'VA' or state = 'CO' or state = 'FL'
-- we can specify not operator as we want customers not in the mentioned list of states
where state not in ('VA', 'CO', 'FL');


-- execersize
select * from products
where quantity_in_stock in (49, 38, 72);
