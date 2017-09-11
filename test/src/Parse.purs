module Test.Parse where

import Prelude

import Data.Either as E
import SqlSquared (parseQuery, printQuery, SqlQuery)
import SqlSquared.Parser (prettyParse)

import Test.Queries as Q
import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert
import Test.Unit.Console as Console

parseSucc ∷ ∀ e. String → TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
parseSucc s =
  test "parse/success"
  case prettyParse parseQuery s of
    E.Left err → Assert.assert ("\n" <> err) false
    E.Right (sql ∷ SqlQuery) →
      case prettyParse parseQuery (printQuery sql) of
        E.Left err →
          Assert.assert
            ("Failed to print and reparse.\n\n" <>
             "  Original: " <> s <> "\n\n" <>
             "  Printed:  " <> printQuery sql <> "\n\n" <> err) false
        E.Right (sql' ∷ SqlQuery)
          | sql' /= sql →
            Assert.assert
              ("Failed to parse to an equivalent AST.\n\n" <>
               "  Original: " <> s <> "\n\n" <>
               "  Parsed:   " <> printQuery sql <> "\n\n" <>
               "  Printed:  " <> printQuery sql') false
          | otherwise →
            Assert.assert "OK!" true

parseFail ∷ ∀ e. String → TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
parseFail s =
  test "parse/fail"
  case parseQuery s of
    E.Left err → pure unit
    E.Right (sql ∷ SqlQuery) → Assert.assert s false

testSuite ∷ ∀ e. TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testSuite = suite "parsers" do
  testSuite1
  testSuite2
  testSuite3
  testSuite4
  testSuite5
  testSuite6

testSuite1 ∷ ∀ e. TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testSuite1 = do
  parseSucc """
    select a from `/f` where b is not between 1 and (2..3)
  """

  parseSucc """
    select foo, bar from (select foo, bar from (select foo, bar from `/baz`)as t) as d
  """

  parseSucc """
    select (1 + 2) * 3 + 2
  """

  parseSucc """
    select 1 + 2 * 3 ^ 1.2 / 14 + Minimum(12, 23)
  """

  parseSucc """
    select ("12" || "12" || "12")
  """

  parseSucc """
    select date("12-12-12") from `/fo` cross join `/bar`
  """

  parseSucc """
    Select foo as bar from `/test/db/collection`
  """

  parseSucc """
    Select `foo`, `bar`[*] from `/test` join `/test2` on baz where doo = (12 + 23)
  """

  parseSucc """
    foo := 12; select * from `/test` group by baz
  """

  parseSucc """
    select 1
  """

  parseSucc """
    select (1, 2)
  """

  parseSucc """
    foo := [1, 2]; select 1
  """

  parseSucc """
    foo := 1; bar := 2; select []
  """

  parseSucc """
    select foo from `/bar` order by zoo desc
  """

  parseSucc """
    select distinct a from `/f`
  """

  parseSucc """
    select a from /* trololo */ `/db`
  """

  parseSucc """
    -- comment
    select 12
  """

  parseSucc """
    import foo; select * from `/test`
  """

  parseSucc """
    create function foo(:bar) begin :bar + 2 end; select * from `/test` where foo = foo(42)
  """

  parseSucc """
    select :where
  """

  parseSucc """
    foo.`_id`
  """

  parseFail """
    foo._id
  """

  parseSucc """
    select * from foo JOIN bar on baz
  """

  parseSucc """
    select * from foo FULL JOIN bar on baz
  """

  parseSucc """
    select * from foo FULL OUTER JOIN bar on baz
  """

  parseSucc """
    select * from foo INNER JOIN bar on baz
  """

  parseSucc """
    select * from foo LEFT OUTER JOIN bar on baz
  """

  parseSucc """
    select * from foo LEFT JOIN bar on baz
  """

  parseSucc """
    select * from foo RIGHT OUTER JOIN bar on baz
  """

  parseSucc """
    select * from foo RIGHT JOIN bar on baz
  """

  parseSucc """
    industry
  """

  parseFail """
    select foo as between from bar
  """

  parseSucc """
    select foo as `between` from bar
  """

  parseSucc """
    select foo from :From
  """

testSuite2 ∷ ∀ e. TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testSuite2 = do
  parseSucc """
    SELECT state AS `ResultAlias`, COUNT(*) as cnt FROM zips GROUP BY state ORDER BY state
  """

  parseSucc """
    SELECT state, COUNT(*) AS count, SUM(pop) AS sum, AVG(pop) AS avg, MIN(pop) AS min, MAX(pop) AS max FROM zips WHERE pop > 10000 GROUP BY state ORDER BY max DESC, state OFFSET 1 LIMIT 10
  """

  parseSucc """
    select city, state, sum(pop) as totalPop from zips where city like "BOULDER%" group by city, state
  """

  parseSucc """
    select sum(pop) as Population from zips where city="BOULDER" and state="CO"
  """

  parseSucc """
    select distinct case pop when 0 then "nobody" when 1 then "one" when 2 then "a couple" when 3 then "a few" else "more" end as cardinal, case pop when 1 then 0 when 10 then 1 end as power, case when pop % 2 = 0 then "even" when pop = 1 or pop = 9 then "odd" else "prime" end as parity, case when pop > 5 then pop - 5 end as grade from zips where pop <= 10 order by pop
  """

  parseSucc """
    select distinct state as abbr, count(pop) as quantity, case when state = "CO" then 1 when state = "WA" then 2 when state = "PA" then 3 when state = "VA" then 4 else 100 end as funnel from zips group by state order by funnel, state
  """

  parseSucc """
    select distinct city from zips order by pop desc limit 5
  """

  parseSucc """
    select city, state, sum(pop) as population from zips group by city, state order by population desc limit 5
  """

  parseSucc """
    select day from days where ts >= start_of_day(:start) and ts <= :end
  """

  parseSucc """
    select [ city, pop ] as cp from largeZips
  """

  parseSucc """
    select loc || [1, 2] as arr, city from largeZips
  """

  parseSucc """
    select city, state, loc || [ pop ] as lp from largeZips
  """

  parseSucc """
    select city, pop, pop between 1000 and 10000 as midsized from smallZips
  """

  parseSucc """
    select array_concat(make_array(pop), array_concat(make_array(1), make_array(2))) as arr from largeZips
  """

  parseSucc """
    select :state as state, count(*) as `count` from zips where state = :state
  """

  parseSucc """
    select integer(`_id`) as intId, decimal(`_id`) as decId from smallZips
  """

  parseSucc """
    select integer(`_id`) as intId, decimal(`_id`) as decId, to_string(pop) as popStr, to_string(loc[0]) as locStr, to_string(length(city) < 9) as boolStr from zips
  """

  parseSucc """
    select meta(brewery).id as brewery_meta_id, brewery.name as brewery_name, beer.name as beer_name, beer.brewery_id as beer_brewery_id from `brewery` as brewery join `beer` as beer on meta(brewery).id = beer.brewery_id where beer.name = "Pale"
  """

  parseSucc """
    SELECT META(brewery).id AS brewery_meta_id, brewery.name AS brewery_name, beer.name AS beer_name, beer.brewery_id AS beer_brewery_id, beer.category AS beer_category FROM `brewery` AS brewery JOIN `beer` AS beer ON META(brewery).id = beer.brewery_id WHERE beer.category = "North American Ale"
  """

  parseSucc """
    select meta(brewery).id as brewery_meta_id, brewery.name as brewery_name, beer.name as beer_name, beer.brewery_id as beer_brewery_id from `beer` as beer join `brewery` as brewery on beer.brewery_id = meta(brewery).id where beer.name = "Pale"
  """

  parseSucc """
    select count(distinct sport) as cnt from olympics
  """

  parseSucc """
    select count(distinct *) as cnt from olympics
  """

  parseSucc """
    SELECT AVG(cnt) as measure, state as category FROM (SELECT COUNT(*) as cnt, state, gender FROM patients WHERE codes[*].desc LIKE "%flu%" GROUP BY state, gender ORDER BY COUNT(*) DESC) as meh
  """

  parseSucc """
    SELECT SUM(value) AS measure, timestamp(dt) AS dimension, sensor AS series FROM smalltimeseries GROUP BY sensor, dt ORDER BY sensor ASC, dt ASC
  """

  parseSucc """
    select profile from user_comments where ( userId LIKE "%Dr%" OR profile.name LIKE "%Dr%" OR profile.age LIKE "%Dr%" OR profile.title LIKE "%Dr%" OR comments[*].id LIKE "%Dr%" OR comments[*].text LIKE "%Dr%" OR comments[*].replyTo[*] LIKE "%Dr%" OR comments[*].`time` LIKE "%Dr%")
  """

  parseSucc """
    select distinct * from cities where city = "BOSTON"
  """

  parseSucc """
    select nr, val1 / val2 as d from divide
  """

  parseSucc """
    select nr, val1 / 0 as d from divide
  """

  parseSucc """
    select parents[*]{*} from slamengine_commits
  """

  parseSucc """
    select topObj{*}.botObj{*} from nested
  """

  parseSucc """
    select count(*)/1000, 0 from zips limit 5
  """

  parseSucc """
    select count(*) as cnt from smallZips where pop < :cutoff
  """

  parseSucc """
    select count(*) as cnt from smallZips where pop < 1000 and length(city) < 8
  """

  parseSucc """
    select max(pop) as max_pop, min(city) as min_city from largeZips where length(city) < 6
  """

  parseSucc """
    select count(*) as cnt from zips where state not in ("AZ", "CO")
  """

  parseSucc """
    select distinct discipline from olympics where event like "%pursuit"
  """

testSuite3 ∷ ∀ e. TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testSuite3 = do
  parseSucc """
    select count(*) as cnt from zips where state in ("AZ", "CO")
  """

  parseSucc """
    select * from zips where 43.058514 in loc[_]
  """

  parseSucc """
    select * from zips where pop < loc[1] order by `_id` limit 10
  """

  parseSucc """
    select day from days where `_id` = oid("54b6f430d4c654959e963a62")
  """

  parseSucc """
    select count(*) as cnt from (select * from zips where city like "BOU%" offset 15 limit 10) as x
  """

  parseSucc """
    select * from zips where city like "BOU%" order by pop limit 10
  """

  parseSucc """
    select `_id` as zip from largeZips where not (city not like "BOULD%" or state != "CO" or (pop between 20000 and 40000 and loc[1] != 40.017235))
  """

  parseSucc """
    select city, state from zips where city not like "BOULD%" and pop = 18174
  """

  parseSucc """
    SELECT loc[*] AS loc FROM zips
  """

  parseSucc """
    SELECT loc[*] as coord, `_id` as zip FROM zips
  """

  parseSucc """
    select parents[0]{*} as parent from slamengine_commits
  """

  parseSucc """
    SELECT `_id` as zip, loc as loc, loc[*] as coord FROM zips
  """

  parseSucc """
    select distinct commit{*}, count(*) from slamengine_commits where commit{*} like "http%" group by committer.login
  """

  parseSucc """
    select commit.author{*} from slamengine_commits
  """

  parseSucc """
    select geo{*} from usa_factbook
  """

  parseSucc """
    select * from nested_foo where foo{*} = 15
  """

  parseSucc """
    select TO_STRING(city), state || "S" from smallZips
  """

  parseSucc """
    select count(*) as cnt from zips where pop < 5 group by pop
  """

  parseSucc """
    select * from zips where `_id` like "8030%" group by city
  """

  parseSucc """
    select distinct count(*) as cnt from slamengine_commits group by parents
  """

  parseSucc """
    select substring(city, 0, 1) as `first`, count(*) as numZips from zips group by substring(city, 0, 1)
  """

  parseSucc """
    select substring(parents[*].sha, 0, 1), count(*) from slamengine_commits group by substring(parents[*].sha, 0, 1)
  """

  parseSucc """
    select distinct * from zips where pop ~* "foo"
  """

  parseSucc """
    select `_id` as zip from zips where city = "BOULDER" and state = "CO"
  """

  parseSucc """
    select foo ?? pop as p, city ?? false as c from zips
  """

  parseSucc """
    select avg(pop) as avg_pop, sum(pop) as tot_pop from zips where city = "BOULDER" and state = "CO"
  """

  parseSucc """
    select * from zips where state in "ME" and pop < 10
  """

  parseSucc """
    select z1.city, z2.state from `../extraSmallZips` as z1 join `../smallZips` as z2 on z1.`_id` = z2.`_id` join `../zips` as z3 on z2.`_id` = z3.`_id`
  """

  parseSucc """
    select smallZips.city, zips.state from `../smallZips`, `../zips` where smallZips.`_id` = zips.`_id` and "MA" = zips.state
  """

  parseSucc """
    select smallZips.city, zips.state from `../smallZips`, `../zips` where smallZips.`_id` = zips.`_id`
  """

  parseSucc """
    select a.city as a, b.city as b, b.pop - a.pop as diff from `../zips` as a, `../largeZips` as b where a.`_id` like "80301" and b.`_id` like "95928"
  """

  parseSucc """
    SELECT p.author.login, COUNT(*) as count FROM `../slamengine_commits` as p INNER JOIN `../slamengine_commits_dup` as c ON p.sha = c.sha GROUP BY p.author.login
  """

  parseSucc """
    select smallZips.city, zips.state from `../smallZips` join `../zips` on smallZips.`_id` = zips.`_id`
  """

  parseSucc """
    select * from `../smallZips` join `../zips` on smallZips.`_id` = zips.`_id`
  """

  parseSucc """
    select t1.city, t1.popCA, t2.popMA from (select city, sum(pop) as popCA from `../zips` where state = "CA" group by city order by sum(pop) desc limit 200) as t1 join (select city, sum(pop) as popMA from `../largeZips` where state = "MA" group by city order by sum(pop) desc limit 200) as t2 on t1.city = t2.city
  """

  parseSucc """
    select l.sha as child, l.author.login as c_auth, r.sha as parent, r.author.login as p_auth from `../slamengine_commits` as l join `../slamengine_commits_dup` as r on r.sha = l.parents[0].sha and l.author.login = r.author.login where r.author.login || "," || l.author.login = "jdegoes,jdegoes"
  """

  parseSucc """
    select l.sha as child, l.author.login as c_auth, r.sha as parent, r.author.login as p_auth from `../slamengine_commits` as l join `../slamengine_commits_dup` as r on r.sha = l.parents[0].sha and l.author.login = r.author.login
  """

  parseSucc """
    select l.sha as child, l.author.login as c_auth, r.sha as parent, r.author.login as p_auth from `../slamengine_commits` as l join `../slamengine_commits_dup` as r on r.sha = l.parents[0].sha
  """

  parseSucc """
    SELECT smallZips.city AS CitySmall, smallZips.state AS StateSmall, extraSmallZips.city AS City, extraSmallZips.state AS State, extraSmallZips.pop AS Pop FROM `../smallZips` JOIN `../extraSmallZips` ON smallZips.pop = extraSmallZips.pop WHERE extraSmallZips.state = "MA"
  """

  parseSucc """
    select z1.city as city1, z1.loc, z2.city as city2, z2.pop from `../zips` as z1 join `../zips` as z2 on z1.loc[*] = z2.pop
  """

  parseSucc """
    select z1.`_id` as zip, z2.pop / 1000 as popK from `../smallZips` as z1 inner join `../zips` as z2 on z1.`_id` = z2.`_id` order by popK desc
  """

  parseSucc """
    SELECT owner.name, car.name from `../owners` as owner join `../cars` as car on car.`_id` = owner.carId
  """

  parseSucc """
    SELECT owner.name, car.name from `../owners` as owner join `../cars` as car on car.`_id` = owner.carId and owner.year = car.year[0]
  """

  parseSucc """
    select a.city, b.state from `../largeZips` as a, `../largeZips` as b where a.`_id` = b.`_id` and "CA" = b.state
  """

  parseSucc """
    select l.sha as child, l.author.login as c_auth, r.sha as parent, r.author.login as p_auth from `../slamengine_commits` as l join `../slamengine_commits` as r on r.sha = l.parents[0].sha and l.author.login = r.author.login where r.author.login || "," || l.author.login = "jdegoes,jdegoes"
  """

  parseSucc """
    select z1.city as city1, z1.loc as loc1, z2.city as city2, z2.loc as loc2 from `../zips` as z1 join `../largeZips` as z2 on z1.loc[*] = z2.loc[*] where z1.city < z2.city
  """

  parseSucc """
    select city, pop from largeZips where length(city) < 5 and pop < 30000
  """

  parseSucc """
    select city, state, sum(pop) as total from zips group by city, state order by sum(pop) desc limit 10
  """

testSuite4 ∷ ∀ e. TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testSuite4 = do
  parseSucc """
    select city, pop from zips where pop > 90000 order by city, pop desc
  """

  parseSucc """
    min_pop := 10000; max_pop := 20000; select zips.city, zips.pop from zips where zips.pop > min_pop and zips.pop < max_pop
  """

  parseSucc """
    cityzips := select zips.city, zips.state from zips where zips.state = "CT"; select city, state from cityzips where city = "SOUTHBURY"
  """

  parseSucc """
    select (smallZips := select * from cars; select * from smallZips) as result from smallZips
  """

  parseSucc """
    select * from zips where city like ("%" || :substr || "%")
  """

  parseSucc """
    select max((0, 1, -2, 5)) as maxlit from cars
  """

  parseSucc """
    select count(PositionHeader.PositionLocation.LocationCity) as counter, PositionHeader.PositionLocation.LocationCity as location from jobs_jobinfo group by location order by counter desc limit 10
  """

  parseSucc """
    select distinct city, length(city) as len from largeZips where state="CO" and length(city) >= 10
  """

  parseSucc """
    select city as a, city as b, city as c, city as d, city as e, city as f from zips where `_id` = "80301"
  """

  parseSucc """
    select * from zips where city ~* "^bOu"
  """

  parseSucc """
    select city from zips where city not like "%E%"
  """

  parseSucc """
    select city from zips where city like "%OULD%CIT%"
  """

  parseSucc """
    select city from zips where city ~ "OULD.{0,2} CIT"
  """

  parseSucc """
    select distinct city, city ~ "OUL" as a, "some text with BOULDER in it" ~ city as b from largeZips where city ~ "^B[^ ]+ER$" and "BOULDER or BEELER" ~ city
  """

  parseSucc """
    select distinct * from `../cities` where city = "BOSTON"
  """

  parseSucc """
    select oid(bar) from `../objectids`
  """

  parseSucc """
    select day from `../../days` where ((ts > date("2014-08-17") and ts <= date("2014-08-20")) and ts != date("2014-08-19")) or ts = date("2014-08-22")
  """

  parseSucc """
    select day, time_of_day(ts) as tod, time_of_day(day) as `not a date`, time_of_day(missing) as missing from `../../days` where time_of_day(ts) >= time("08:00") and time_of_day(ts) < time("10:20:30.400")
  """

  parseSucc """
    select date(substring(str, 0, 10)), time(substring(str, 11, 20)), timestamp(str), to_string(ts) from `../../days`
  """

  parseSucc """
    select count(*) from slamengine_commits where commit.message like "Merge%"
  """

  parseSucc """
    select * from nested_foo where ( foo LIKE "%zap%" OR foo[*] LIKE "%15%" OR foo[*][*] LIKE "%meh%" OR foo[*][*].baz LIKE "%moo%" OR foo[*][*].baz[*] LIKE "%quu%" OR a LIKE "%13%")
  """

  parseSucc """
    select * from nested_foo where ( foo{*} LIKE "%15%" OR foo{*} = 15 OR foo[*] LIKE "%15%" OR foo[*] = 15)
  """

  parseSucc """
    select city, city !~ "A" as noA from largeZips where city !~ "CHI"
  """

  parseSucc """
    select distinct (val is not null) as expr from nulls where val is not null
  """

  parseSucc """
    select distinct (val != null) as expr from nulls where val != null
  """

  parseSucc """
    select distinct (val is null) as expr from nulls where val is null
  """

  parseSucc """
    select name, val, val = null, val is null, val is not null, nested.val as nval, nested.val = null, nested.val is null, nested.val is not null from nulls
  """

  parseSucc """
    select name, val, val = null, val is null, val is not null, nested.val as nval, nested.val = null, nested.val is null, nested.val is not null from nulls
  """

  parseSucc """
    select null(name) as n, to_string(val) as s from nulls where name = "null"
  """

  parseSucc """
    select city as `12`, pop as `42` from smallZips
  """

  parseSucc """
    select *, pop * 10 as dpop from largeZips order by pop / 10 desc
  """

  parseSucc """
    select city, city ~* "boU" as a, city !~* "Bou" as b from largeZips
  """

  parseSucc """
    select intro from usa_factbook where geo{*}.text = "19,924 km"
  """

  parseSucc """
    select (loc || [1, 2])[1] from largeZips
  """

  parseSucc """
    select ([6, 7] || loc)[1] from largeZips
  """

  parseSucc """
    select parents[0].sha, count(*) as count from slamengine_commits group by parents[0].sha
  """

  parseSucc """
    select parents[0].sha, count(*) as count from slamengine_commits where parents[0].sha like "5%" group by parents[0].sha
  """

  parseSucc """
    select parents[0] as parent, commit{*} from slamengine_commits
  """

  parseSucc """
    select city, loc[0] as lat from largeZips
  """

testSuite5 ∷ ∀ e. TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testSuite5 = do
  parseSucc """
    select committer.login, count(*) from slamengine_commits
  """

  parseSucc """
    select city as a, city as b, city as c, city as d, city as e, city as f, city as g, city as h, city as i, city as j, city as k, city as l, city as m, city as n, city as o, city as p, city as q, city as r, city as s, city as t from largeZips
  """

  parseSucc """
    select *, pop/1000 as popInK from largeZips
  """

  parseSucc """
    select distinct city from zips order by city
  """

  parseSucc """
    select "servlet-name", "init-param" is not null from webapp where "init-param" is null or "init-param"."betaServer"
  """

  parseSucc """
    select distinct city from largeZips order by length(city), city limit 5
  """

  parseSucc """
    select distinct discipline, event from olympics where event like "%pursuit"
  """

  parseSucc """
    select city from largeZips where length(city) < 5
  """

  parseSucc """
    select `_id` as zip, city from zips where pop = 18174
  """

  parseSucc """
    select * from zips where state in ("MA") and pop < 10
  """

  parseSucc """
    select distinct discipline as d from olympics
  """

  parseSucc """
    select distinct discipline from olympics order by discipline
  """

  parseSucc """
    select count(*) from (select * from zips offset 10) as x
  """

  parseSucc """
    select city, pop from zips where pop > 110000 order by length(city)
  """

  parseSucc """
    select city, state from zips order by city, state limit 10
  """

  parseSucc """
    select * from largeZips order by pop/10 desc
  """

  parseSucc """
    select state, min(city) as first, length(min(city)) as len from zips group by state order by len, first, state limit 5
  """

  parseSucc """
    select day, ts, to_timestamp(epoch) as converted from `../days` where ts = to_timestamp(1408255200000) or to_timestamp(epoch) = timestamp("2014-08-18T07:00:00Z")
  """

  parseSucc """
    select distinct `day` from `../days` where date_part("dow", ts) >= 3
  """

  parseSucc """
    select date_part("millennium", timestamp(commit.committer.date)) as millennium, date_part("century", timestamp(commit.committer.date)) as century, date_part("decade", timestamp(commit.committer.date)) as decade, date_part("year", timestamp(commit.committer.date)) as year, date_part("quarter", timestamp(commit.committer.date)) as quarter, date_part("month", timestamp(commit.committer.date)) as month, date_part("day", timestamp(commit.committer.date)) as dayOfMonth, date_part("dow", timestamp(commit.committer.date)) as dayOfWeek, date_part("isodow", timestamp(commit.committer.date)) as `dayOfWeek (ISO)`, date_part("hour", timestamp(commit.committer.date)) as hour, date_part("minute", timestamp(commit.committer.date)) as minute, date_part("second", timestamp(commit.committer.date)) as second, date_part("milliseconds", timestamp(commit.committer.date)) as millis, date_part("microseconds", timestamp(commit.committer.date)) as micros, to_string(author.id) as id from `../slamengine_commits`
  """

  parseSucc """
    select date_part("day", timestamp(commit.committer.date)) as dayOfMonth, to_string(author.id) as id from `../slamengine_commits`
  """

  parseSucc """
    select distinct day, date_part("dow", ts) as dow, date_part("isodow", ts) as isodow from `../days`
  """

  parseSucc """
    select day from `../days` where ((date_trunc("day", ts) > start_of_day(date("2014-08-17")) and date_trunc("day", ts) <= start_of_day(date("2014-08-20"))) and date_trunc("day", ts) != start_of_day(date("2014-08-19"))) or date_trunc("day", ts) = start_of_day(date("2014-08-22"))
  """

  parseSucc """
    select day from `../days` where date_part("dow", epoch) >= 3
  """

  parseSucc """
    select day, time_of_day(ts) as tod, time_of_day(day) as notadate, time_of_day(missing) as missing from `../days` where time_of_day(ts) >= time("08:00") and time_of_day(ts) < time("10:20:30.400")
  """

  parseSucc """
    select day, (ts - timestamp("2014-08-17T00:00:00.000Z")) / interval("PT1H0M0S") as hoursSinceSunday from `../days` where ts < timestamp("2014-08-17T12:00:00Z") or ts - interval("PT12H") > timestamp("2014-08-22T00:00:00Z")
  """

  parseSucc """
    select date(substring(str, 0, 10)) as d, time(substring(str, 11, 8)) as t, timestamp(str) as ts, to_string(ts) as s from `../days`
  """

  parseSucc """
    select topObj{*}{*}{*} from nested
  """

  parseSucc """
    select * from smallZips
  """

  parseSucc """
    select city, sum(pop) as totalPop from largeZips group by city
  """

  parseSucc """
    select foo, type_of(foo) as type from nested_foo
  """

  parseSucc """
    select distinct length(meh) as meh_len, meh.mep, length(meh.mep) as mep_len from zips
  """

  parseSucc """
    select loc[*] from zips where loc[*] < 0
  """

  parseSucc """
    select `_id` as zip, loc[*] from zips where loc[*] > 68 order by loc[*]
  """

  parseSucc """
    select `_id` as zip, loc[*] from zips order by loc[*] limit 10
  """

  parseSucc """
    select `_id` as zip from zips union select city, state from zips
  """

  parseSucc """
    select state, city, [pop ...] as pop from zips group by state, city
  """

  parseSucc """
    select count(*) as cnt from :table where pop < 1000
  """

  parseSucc """
    mystate := "CO"; select * from zips where state = mystate
  """

  parseSucc """
    select *, concat(city, concat(", ", state)) as city_state, city = "BOULDER" as boulderish from zips where city like "BOULDER%"
  """

  parseSucc """
    select *, '1' as one, '2' as two from largeZips
  """

  parseSucc """
    select * from zips order by pop, city limit 10
  """

  parseSucc """
    select length(city) as len, count(*) as cnt from largeZips where state != "MI" group by length(city)
  """

  parseSucc """
    select foo.* from zips as foo
  """

  parseSucc """
    select foo from bar union all select baz from quux
  """

testSuite6 ∷ ∀ e. TestSuite (testOutput ∷ Console.TESTOUTPUT | e)
testSuite6 = do
  parseSucc Q.q1
  parseSucc Q.q2
  parseSucc Q.q3
  parseSucc Q.q4
  parseSucc Q.q5
  parseSucc Q.q6
  parseSucc Q.q7
  parseSucc Q.q8
  parseSucc Q.q9
  parseSucc Q.q10
  parseSucc Q.q11
  parseSucc Q.q12
  parseSucc Q.q13
  parseSucc Q.q14
  parseSucc Q.q15
  parseSucc Q.q16
  parseSucc Q.q17
  parseSucc Q.q18
  parseSucc Q.q19
  parseSucc Q.q20
  parseSucc Q.q21
