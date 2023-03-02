(*******************************************************************************
Homework 4
==========
CSCI 499, Fall 2022: Introduction to Functional Programming
Mukund Raghothaman
Due: 10pm PT on 30 November, 2022
*******************************************************************************)

(*******************************************************************************
- Your name: __________
- Your partner's name (if any): __________
*******************************************************************************)

(*******************************************************************************
Instructions
------------
1. Please setup your programming environment by following the steps outlined in
   the cheatsheet and on the course website.

2. Please feel free to collaborate with a partner, but make sure to write your
   submission individually.

3. This assignment depends on the SQLite embedded database library. You may
   download a pleasant GUI from which to use SQLite from
   https://sqlitebrowser.org/.

3. This assignment also depends on the Souffle Datalog solver.

   The easiest installation path might be to follow the instructions available
   from https://souffle-lang.github.io/install, in case you are running Debian,
   Ubuntu, or Mac OS X. This might also work if you are running Ubuntu on
   WSL / WSL2.

   Otherwise, you may obtain Souffle from
   https://souffle-lang.github.io/download.html. The latest release is
   Version 2.3 (https://github.com/souffle-lang/souffle/releases/tag/2.1). In
   this case, you may build Souffle by running
   "cmake -S . -B build && cmake --build build -j" at a command prompt. You will
   find the Souffle executable in the directory build/src.

   I have had problems building this version of Souffle on some computers. In
   this case, you may freely use the previous version, Version 2.0.2
   (https://github.com/souffle-lang/souffle/releases/tag/2.0.2). To build,
   simply run "./bootstrap; ./configure; make". You will find the Souffle
   executable in the directory src.

3. There are four files in this assignment. When complete, place them in a
   directory named "HW4-Yourname", compress it into a zip archive,
   "HW4-Yourname.zip", and submit your assignment using Blackboard.
*******************************************************************************)

open Base

(** Distribution of points: 10 + 10 + 10 + 10 + 20 = 60 points *)

(******************************************************************************)

(* Question 1: Crashing Computers and Overdue Assignments (10 points)

   Here is data about the traffic in New York City at a particular moment in
   time. There are three tables:
   1. The table Intersects summarizes the city's geography, and contains all
      tuples (x, y) such that there is an intersection between x and y. Of
      course, this table is commutative, so for every tuple (x, y) in it, it
      also contains the tuple (y, x).
   2. The table HasTraffic contains a single column, and lists all those streets
      with heavy traffic at that moment.
   3. The table GreenLight also contains only a single column, and lists all
      those streets where the traffic light is green.

   Intersects(s1, s2)
   -----------------------
   Broadway   | Liberty St
   Liberty St | Broadway
   Liberty St | William St
   William St | Liberty St
   William St | Wall St
   Wall St    | William St
   Wall St    | Broadway
   Broadway   | Wall St
   Broadway   | Whitehall
   Whitehall  | Broadway

   HasTraffic(s)
   -------------
   Broadway
   Wall St
   Whitehall
   William St

   GreenLight(s)
   -------------
   Broadway
   Liberty St
   Whitehall
   William St

   Your friend is taking a course on urban planning, and wrote a SQL query which
   computed the following output:

   Mystery(s1, s2)
   ---------------------
   Broadway  | Whitehall
   Whitehall | Broadway

   Unfortunately, their computer crashed, and the assignment is due today. What
   SQL query could they have written? Please test your response using the
   attached SQLite database.

   SELECT s1, s2
   FROM Intersects
   WHERE 
   s1 IN (SELECT s FROM HasTraffic) 
   AND
   s2 IN (SELECT s FROM HasTraffic) 
   AND
   s1 IN (SELECT s FROM GreenLight) 
   AND
   s2 IN (SELECT s FROM GreenLight) 

   Bonus points: What could this query have been about?
   The query is searching for all intersections which have both traffic and green
   light.
   *)





(******************************************************************************)

(* Question 2: Understanding SQL queries (10 points)

   Consider the following pairs of SQL queries. Will they produce the same
   result on all databases, or can you provide an example database on which
   their outputs differ? Justify your answers. You may assume that the tables r
   and s have the same schema.

   1. select *
      from (select * from r) union all (select * from s)
      where p

      and

      (select * from r where p)
      union all
      (select * from s where p)

      These two would produce the same result, but the second one is more efficient.
      In the first case, the databases are first merged and then they are inspected
      to select those rows that follow condition p. In the second one first the rows
      that follow condition p are selected from r and s independelty, to then join
      them. As a result, the output is the same.

   2. (select * from r where p)
      union all
      (select * from s where p)

      and

      (select * from r)
      union all
      (select * from s where p) 
      
      These would not produce the same results.
      The first one would include all rows that follow p condition from r and s. The
      second one would include all rows from r, and the rows that follow p from s.
      Therefore, the second result could include more rows.

      Example:
      p: c2 > 5
      r:
      a  |  1
      b  |  4
      c  |  6
      d  |  3
      s:
      a  |  6
      b  |  2
      c  |  1
      d  |  9

      First query:
      a  |  6
      c  |  6
      d  |  9

      Second query:
      a  |  1
      b  |  4
      c  |  6
      d  |  3
      a  |  6
      d  |  9
      *)





(******************************************************************************)

(* Question 3: Writing Queries in Cypher (5 + 5 = 10 points)

   You may experiment with Neo4j in a sandboxed environment at the following
   website: https://sandbox.neo4j.com/. For this question, we will focus on the
   Movies sample database.

   1. Find all actors who acted both in movies released before 2000, and after
      2010. State the queries you used to find your answer.
          MATCH 
         (m1:Movie WHERE (m1.released < 2000))
         <-[r1:ACTED_IN]-(p:Person)-[r2:ACTED_IN]->
         (m2:Movie WHERE (m2.released > 2010))
         RETURN p, m1, m2

         or RETURN DISTINCT p.name if you only want a list of the names.

   2. Identify the actor with the longest career, and describe the query you
      used to obtain your answer.
       Jack Nicholson, active for 29 years.

         MATCH (p:Person)-[:ACTED_IN]->(m:Movie)
         WITH p, max(m.released) as max, min(m.released) as min
         RETURN p, max-min+1 as yearsActive
         ORDER BY yearsActive DESC
         LIMIT 1
       *)




       
(******************************************************************************)

(* Question 4: Genealogies (5 + 5 = 10 points)

   Please follow the instructions in the attached file q4.dl. *)




(******************************************************************************)

(* Question 5: Incremental Evaluation, Redux (5 + 8 + 7 = 20 points)

   Please follow the instructions in the attached file q5.dl. *)
