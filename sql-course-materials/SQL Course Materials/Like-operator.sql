select * from customers
		-- like operator is used for specifying strings which starts with particular string or character and % means any number of characters after that.
-- where last_name like 'b%';
-- where last_name like 'boa%';
		-- here we have specfied %b% so this means we want to get customer who has b in his last_name at any position
		-- it diesn't matter b in the starting or ending or in the middle
-- where last_name like '%b%';
      -- here we have givem _ which means exact one character so in last_name any one character and it shoud be end with y
-- where last_name like '_y';
		-- here we have given 5 underscores so last name should contain any 5 characters and end with y
where last_name like '_____y';


-- execersize
select * from customers
where address like '%trail%' or address like '%avenue%';

select * from customers
where phone not like '%9';
