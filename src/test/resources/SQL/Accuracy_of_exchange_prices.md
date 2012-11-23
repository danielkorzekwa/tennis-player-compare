Accuracy of exchange prices
---------------------------

### 1) Accuracy of all exchange prices

	select cast(1/odds*100 as int) as predicted_prob,(cast(sum(cast(win_flag as int)) as real)/COUNT(*))*100 as true_prob,COUNT(*) as sample_size 
	from dbo.betfair_data group by cast(1/odds*100 as int) order by predicted_prob

### 2) Accuracy of starting prices

	--- Get starting market prices
	select * from (select *, row_number() over (partition by event_id,selection_id order by latest_taken desc) as row from betfair_data) A where row=1
	
	--- Get accuracy of prices
	select cast(1/odds*100 as int) as predicted_prob,(cast(sum(cast(win_flag as int)) as real)/COUNT(*))*100 as true_prob,COUNT(*) as sample_size 
	from (select price.* from market_prices() price) A 
	group by cast(1/odds*100 as int) order by predicted_prob