CHAPTER 1
Building Scalable Systems with Erlang
and REST
In the early days of the Web, building systems was simple. Take a Linux box, put Perl
or PHP on it, add Apache and MySQL, and you were ready to go. Of course, this system
was pretty limited. If you wanted to scale it past one or two servers it got real hard, real
fast. It turns out that building scalable distributed applications is difficult, and the tools
available to build them are often less than ideal.
Over the first decade of the 21st century, companies like Google, Amazon, eBay, and
many others found that they needed to scale not to a few servers but to a few thousand
servers, or even tens or hundreds of thousands or more. This requires a very different
way of thinking about how to build a system, and dropping many of the patterns that
had been used in the past for smaller systems.
One alternate recipe that offers scalability, resilience, and flexibility is to create your
sites and applications using Erlang, with the frontend being defined by a variety of web
services.
Why Erlang?
When I was getting ready to write this book I described the idea to several programmer
friends. They all said, “I would never think of building a large-scale website in Erlang.”
It may seem daunting, but Erlang has features that fit large-scale web projects perfectly.
Ericsson originally created Erlang, a functional language based on Prolog, to run in
telecom switches in the 1980s. Telecom switches must run without interruption for
long periods of time, and this drove many of the choices that were made in Erlang. It
was built to support systems that would have to be fault tolerant and able to be up-
graded in place without downtime. As it turns out, these features are ideal not only for
telephone switches, but also for business-critical web services.
1
One of the first major projects to use Erlang was the Ericsson AXD301 switch, which
used about a million lines of Erlang code along with some device drivers and other low-
level components that were written in C. The AXD301 switch has achieved an unpre-
cedented level of reliability in the field—in some cases, it has achieved “nine 9s” reli-
ability!1 The amount of time that the system could be expected to be offline could be
measured in milliseconds per year. (This was for the entire network, not a single node.)
Clearly, most systems written in Erlang will not achieve that level of reliability. With
careful design and testing, it’s possible for a system to hit six 9s (about 30 seconds of
downtime per year). However, reaching that is beyond the scope of this book, and
requires a very careful study of risks that may cause the system to be unavailable and
ensuring that no single failure (in particular, beyond your code) could cause that. For
example, having three connections to the Internet with different ISPs is great, but if all
three go through the same conduit it only takes one guy with a backhoe to cut all three
wires and take a system offline.
Erlang applications can be upgraded in place. If an application is running on a cluster
of servers and a bug is discovered in one module, there is no need to stop the system
to upgrade to a new version of the software—Erlang provides a method to upgrade the
code as it runs so that customers never need to be interrupted. This is a major advantage
over a system where an application needs to be offline for an hour or more each time
a new version of the software is rolled out, costing real money as customers are not able
to use the system.
Erlang is also designed to support clusters of computers. In fact, to have a scalable and
fault-tolerant system, it must run on more than one computer. As any given computer
can fail, it is important that the system be able to deal with the case of a node in the
cluster going offline and still providing services to the customers. How many nodes a
system should run on is a complex issue, but it starts with the question “What is the
probability of all the remaining nodes failing before I can bring a new node online?”
If you Google “Erlang”, you will see references to “Erlang-B” and “Er-
lang-C”. These are measures of telephone capacity that are probably of
great importance if you are building a call center, but have nothing to
do with the programming language.
Erlang’s Advantages
Erlang does many things differently. In most programming languages, concurrency is
an afterthought. Each process in PHP, for example, runs independently and generally
communicates with other PHP processes only via external resources like a database or
memcached server. In Erlang, concurrency is built in from the very base of the system.
1. In practice, this often means “The system was more reliable than our way of measuring it.”
2 | Chapter 1: Building Scalable Systems with Erlang and REST
Another difference is that Erlang is a compiled language. In PHP you can just edit a file
and go to the web server, and it will be running the new version. In Erlang you need to
compile the code and load it into the system, and while this is not difficult, it does
represent an extra step.
Perhaps the strangest thing about Erlang for a new Erlang programmer is that all vari-
ables are single assignment. In Java terms, it’s as if all variables are final. This takes
some time to adapt to, but is in fact quite powerful in a language where concurrent
processing is normal. If a variable can never be changed, then locks become almost an
irrelevant detail. The other advantage is that a single assignment variable can only have
its value assigned in one place, so if it has the wrong value then determining where that
value came from becomes much easier: it must have been set at initial assignment.
Erlang features a message passing model for concurrency, so there is no shared state
between threads—removing the need for a programmer to set locks in code. If you need
shared state, you can do it via the Mnesia database (see “Mnesia” on page 11), Mnesia
supports transactions and locks, providing in effect a form of software transactional
memory (STM) shared memory.
Erlang’s processes are a feature of the language, not the operating system. An Erlang
process is much lighter in weight than a similar OS process. Processes in Erlang com-
municate with each other by sending messages, which generally has very low overhead,
but can be heavy if a large amount of data is being copied between processes.
Unless specified otherwise, “processes” in this book refer to Erlang pro-
cesses, not OS processes. Erlang’s processes are very lightweight and
have very fast switching and startup times.
Lack of Types
Erlang has been criticized for its lack of a type system, and it’s true that Erlang does
not have static typing like Haskell does. Type systems give programmers a way to prove
that the program is consistent in how it treats data. However, in a distributed system
like Erlang, providing that kind of static consistency has some practical costs.
Erlang allows you to upgrade a system while keeping it running. However, by doing
this, you create a system that is inconsistent. If types are changed in a version change
(and it is reasonable to assume that most version changes will involve changing types),
demanding static typing means that nodes running the old version cannot communicate
with nodes running the new version—and the same with processes within the same
node.
Imagine a case where there are just two nodes in a system, both running the same
version of some software. This is a consistent system, where the consistency is one of
type definition. However, when it comes time to upgrade the system, there will be a
Why Erlang? | 3
period of time when one node is running the new software and the other is running the
old software. At this point you have an inconsistent system with regard to types.
At this point you have a few options. If you had built your system in Haskell, you would
probably need to have a partition in which nodes running the old version of the software
could not talk to those running the new version. You could also just take the system
down for a short period of time while you did the upgrade, therefore sacrificing the
availability of the system but ensuring that the system while running is never partitioned
and never inconsistent.
There is no general perfect solution to this problem. Erlang was built to optimize for
maximum availability, as choices were made to allow it to be inconsistent in some ways
while still making services available. It may in fact be possible to solve this in Haskell,
but thus far no one has done so. Erlang was built with the assumption that errors will
happen and that the system should have methods of dealing with them on an ongoing
basis. Haskell was built to minimize errors, period. Different priorities led to different
designs.
OTP—For More Than Just Telecom!
The Open Telecom Platform (OTP) framework for building fault-tolerant applications
ships with Erlang. By setting up software to run inside the OTP framework, applications
can take advantage of OTP’s built-in fault recovery and monitoring. OTP automates
much of the concurrency of Erlang, but what really makes it shine is its ability to mon-
itor a running application and keep it running.
Erlang code takes a “let it crash” approach, unlike the try/catch blocks in many other
languages. Erlang figures that when something goes wrong, let it go wrong, and don’t
try to duct tape it back together in an unknown state. OTP will restart monitored
processes that die. This also has the benefit that a process that is on a node that has
died can be restarted elsewhere. (Obviously a node cannot fix itself if the server it is on
has died.) If you want a system that can be fault tolerant and continue to provide your
service, you want a framework that can deal with failure and simply work around it.
This book builds an application using OTP in Chapter 9; however, this is not a complete
introduction to the subject as I cover only the elements that are needed to write this
specific application. The books Erlang Programming and Programming Erlang both
provide a more detailed introduction, while the book Erlang and OTP in Action goes
into much greater detail on OTP.
Why Web Services? Why REST?
Years of work with the Web have made people comfortable with the idea that a specific
URL is tied to a specific resource. For example, the URL http://en.wikipedia.org/wiki/
Erlang_(programming_language) is the Wikipedia page on Erlang. It is obvious in this
4 | Chapter 1: Building Scalable Systems with Erlang and REST
case how the URL relates to the underlying resource. For a web page meant to be read
by a person with a web browser, this is a useful representation.
Before REST surfaced, emerging from careful study of how and why HTTP succeeded,
developers created a number of ways to send a remote procedure call over a network.
When HTTP became the dominant mechanism for Internet communications, many of
those same mechanisms were repurposed to run over HTTP. This made broad sense,
as HTTP tools are common, but didn’t always take advantage of HTTP’s strengths.
Prior to REST, people tended to tunnel services over SOAP. However, SOAP does not
make very good use of HTTP—it sends only XML messages back and forth over HTTP
POST requests. It doesn’t take advantage of caching proxies or other features of the
HTTP infrastructure, beyond HTTP’s frequent ability to go through a firewall.
REST takes much better advantage of HTTP, using HTTP’s limited set of request verbs
and living within the expectations for their processing. This forces an approach of
working with a limited number of actions on an unlimited number of possible resour-
ces. It takes some getting used to, but it offers a consistent and powerful way to send
information across networks that it easily integrated with web infrastructure and
interfaces.
For full details on how a REST service should work, take a look at REST
in Practice by Webber, Parastatidis, and Robinson (http://restinpractice
.com).
REST treats URLs—usually called Uniform Resource Identifiers (URIs) in this context
—as the fundamental way to address an underlying resource. Furthermore, a resource
may have several representations; so for example, an ebook may be accessible as a PDF,
mobi, or some other format.
In a RESTful service, the four HTTP verbs GET, POST, PUT, and DELETE have well defined
meanings. A GET request should only retrieve information. A GET should also be idem-
potent: a client can call it as many times as needed, and it will not change the state of
the system in any way the client will care about. (For example, it may add information
to logs, but not change user-facing data.) As long as the server sets an ETag or a Cache-
Control header, this makes it easy for a proxy server or client to cache a resource,
allowing much faster response on reads across a network. (HEAD and OPTIONS requests,
if you use them, should also be idempotent.)
The POST method will create a new entity, which could be a chatroom or a record in a
database. The PUT method will replace a resource with a new version. This can be used
to update records or the like. The DELETE method is used to remove a resource.
REST defines the DELETE and PUT methods so that they are repeatable. That is to say,
calling them several times will have the same effect on a system as calling them once.
Why Web Services? Why REST? | 5
For example, if you call DELETE on a resource one time or four, it should still have the
end result that the resource is deleted (or an error is generated).
In a RESTful service the URL should reliably serve to identify the resource to be worked
on. In many ways, you’ll want to build by identifying your resources first, and then
figuring out how the interactions mesh to create an application.
New Opportunities for Scaling and Resilience
Erlang and RESTful web services fit into a larger picture of recent technical changes
that make it easier to apply Erlang’s strengths.
Cloud Computing
Cloud computing, at least on the “Infrastructure as a Service” (IaaS) model, makes
adding a new server to a network easy and fast. In a pre-cloud system, adding a new
server would require ordering it, going to the data center, and physically installing it in
a rack. Most cloud setups reduce that to a REST API that can start up a server in a
minute or two.
This complements Erlang perfectly. Erlang has lots of features that allow a networked
system to add nodes in real time and to detect when they fail. Of course, the specifics
of how to set up an Erlang application in the cloud will depend a lot on the details of
the application and what kind of loading it is expected to get.
In IaaS cloud implementations the service provides virtual platforms,
each of which runs a full OS. For use with Erlang that would probably
be some form of Linux, but could also be Windows or some other OS.
Erlang provides a built-in function (BIF) called erlang:monitor_node/2 that will send a
message of the form {nodedown, Node} if the node in question goes offline. It would be
simple to have the monitoring process use the REST API from AWS or another cloud
provider to automatically bring up a new node in this case. It would also be possible
to have the system bring up new nodes if the system is becoming overloaded.
There are two times when a system may wish to bring up one or more nodes. The first
is when a node fails, and the system brings up a new node to replace it. The second is
when a set of nodes is getting overloaded. This will of course take some system moni-
toring. But if a system is smart enough to know that the average system load over a set
of nodes is increasing, then instead of crashing and letting the admin deal with it later,
the system can be set up to create new nodes and link them into the system. The details
of how to do this will vary depending on the hosting provider and the needs of the
application.
6 | Chapter 1: Building Scalable Systems with Erlang and REST
It is probably also smart to include an option to override the automatic system and
allow an admin to set a number of servers manually. For example, if your company is
going to run an ad in the Super Bowl,2 then it makes sense to have enough servers
running and ready before the ad runs and the systems overload.
In addition to scaling out, there is also the issue of scaling down during those times
when a system has more nodes than are needed. Your system may have been running
up to 300 nodes to handle the load from the Super Bowl advertisement, but now that
it’s over it can be scaled back to a lower level. This is also useful for running the appli-
cation on a test machine in development.
System Architecture and Erlang Scaling
From about 1970 to about 2002, system processors got faster, doubling in speed every
18 months or so. However, somewhere around 2002 something changed. As speeds
kept getting faster, the laws of physics threw a brick in this progress. Faster speeds
generate more heat, which uses more power and causes more problems in getting rid
of waste heat. In addition, the speed of light puts a hard limit on how far a signal can
travel in one clock cycle. Therefore, since 2002 the trend has not been to make pro-
cessors faster but to put more of them on each chip.
When the CPUs were getting faster, it was pretty easy to speed up your code. If you
just waited 18 months and did nothing, your program would go twice as fast! In the
age of multi-core processors, this no longer works. Now programmers need to write
programs that will use all the cores on a system. On a six-core chip, a sequential program
can be running full steam on one core, but the other five are sitting around doing
nothing.
As of the fall of 2011, Intel’s high-end server chips have eight cores, the consumer chips
from Intel have up to six cores (in many of those cases, each core can run two threads),
and AMD has announced a line of processors with eight cores. IBM’s Power7 chip has
eight cores that run four threads each. It is not crazy to expect that in a few years we
will be talking about chips with 32, 64, or even 128 cores or more. The way we write
programs for these processors will be different from the way we wrote programs for the
single-processor chips of the past. It is not clear that Erlang will scale to 64 or 128 cores,
but it probably has a better chance to do so than most other languages.
If you want to use a multi-core chip efficiently, you need a large number of processes
ready to run. Ideally the number of processes should be much larger than the number
of chips to simplify distribution. If there are 16 processor threads running on the CPU,
having only 16 or 32 processes will probably not work well, as statistically there needs
to be a pool of processors waiting to run so that there is never a time when all the
processes are blocked. There will be many times when the chip is doing nothing while
2. For those of you outside North America, the Super Bowl is the biggest festival of advertising in the United
States each year. It also features a sporting event.
New Opportunities for Scaling and Resilience | 7
processes are waiting on the disk or network or the like. Having a large number of
processes waiting means that the system can always have tasks in the queue when one
process goes into a wait state.
Assuming that the time to switch between processes is very small (which for Erlang
processes it is) then having several thousand processes or more would be best, so the
system can make sure there are always processes to be thread into a waiting core.
The ability of a system like Erlang to scale well is dependent on three things: the speed
at which processes are started, the speed at which the system can switch between them,
and the cost for passing messages. Erlang does a good job minimizing all three of these
factors.
Scaling up versus scaling out
There are two basic ways to scale a system: up or out. To scale a system up means to
replace the server with a larger one—you take out the existing server and add in one
with more CPUs, more memory, more disk, etc. There are limits to this, however, and
it can be expensive. IBM’s top-of-the-line servers can have as many as 32 CPUs with
1024 processor threads running at the same time. In web scale, however, that can still
seem rather small.
To scale a system out means to spread it over a number of smaller servers. So instead
of buying the million-dollar IBM Power7 server, you buy a bunch of Intel class servers
and spread the work across them. The advantage of this is that if set up correctly, there
are no limits besides the budget in how far it can scale. When used with today’s cloud-
based PaaS platforms, it can be possible to scale up for unexpected loads in minutes by
ordering more servers from AWS or another cloud provider.
Amdahl’s law
Gene Amdahl is a computer architect originally known for designing mainframes for
IBM and others from the 1950s to the 1980s. He presented a strong argument about
the nature of systems in which some parts are parallel and other parts are not.
This argument, known as Amdahl's law, states that in a system where parts of the
process are sequential and other parts are parallel, then the total speedup can never be
more than the parts that are sequential—adding more cores won’t make the whole
system go faster. (For a full explanation of Amdahl’s law, see the Wikipedia page on
the subject: http://en.wikipedia.org/wiki/Amdahl%27s_law.)
As an analogy, imagine that you go to a bank in which there are a bunch of tellers but
only one cash counting machine. As more customers come in, the manager can always
add more tellers, but if they must stand in line to use the cash counter the system will
never get faster.
In any application, there will always be parts that are sequential. In an Erlang applica-
tion, a few places come to mind. Module setup and tear down code is sequential, but
8 | Chapter 1: Building Scalable Systems with Erlang and REST
as it will normally be run only when new services are being brought online, it is probably
not a major source of bottlenecks.
One place that sequential resource uses can become a problem is access to disk. Disks
are by definition sequential in that a given disk can be reading or writing only one thing
at a time. The disk is also usually orders of magnitude slower than memory or CPU
cache. Components like data stores that write data to disk or logging modules are often
places where a bottleneck for the whole system can occur.
Another place that can cause a lot of sequential code is locks. In general, this is not an
issue in Erlang the way it would be in Java or C#, but at least in theory it could be an
issue with Mnesia or similar tools if things get blocked waiting for transactions.
Data Storage Options
Back in the “old days” of say, 1998 to 2005, the options for data storage when devel-
oping a web service was a choice of SQL databases. MySQL was always the easy choice;
other options included Postgres, Oracle, and Microsoft SQL Server. All of these prod-
ucts are SQL databases, with all that is good and bad about SQL built into them.
SQL databases are very good for many things, but fail rather badly when it comes to
horizontal scaling. Trying to build a partitioned database or a multi-master setup in
most SQL databases is at best a major pain in the neck and at worst actively difficult.
If Erlang and Yaws have been chosen for a project with the goal of having the service
be fault tolerant and scalable, then of course those properties must be present in the
data storage solution as well.
In the modern age, many web development projects are moving to “NoSQL,” which is
a loosely defined set of data storage technologies that have been created to deal with
web-scale data. The good thing about NoSQL is that there are many more choices in
terms of how data will be stored than there are in SQL. The bad thing is that since there
are many more choices, the team developing an application must be ready to under-
stand those choices and select the system or systems that will work best.
NoSQL solutions lack some SQL features that programmers have become used to. The
first thing to note is that there is no idea of a join in most NoSQL data stores. Trying
to join two tables across multiple hosts is a problematic task, requiring multiple phases
of searching and joining using MapReduce techniques or something similar.
For an overview of a number of SQL and NoSQL databases, check out
the book Seven Databases in Seven Weeks by Eric Redmond and Jim R.
Wilson (Pragmatic Programmers: http://pragprog.com/book/rwdata/
seven-databases-in-seven-weeks). This book discusses PostgreSQL,
Riak, Redis, HBase, MongoDB, CouchDB, and Neo4j.
New Opportunities for Scaling and Resilience | 9
Many NoSQL data stores also lack any concept of a transaction. Ensuring consistency
is up to the programmer. Again, this flows from the distributed nature of the data store.
Trying to ensure that all the data across several hosts is always constant can often be
an O(N) or even O(N^2) task. So it falls to the developer to ensure that data manip-
ulations work in a sensible manner.
The other thing to be aware of when moving from SQL to NoSQL is that finding de-
velopers and system administrators who have been doing SQL for many years is rela-
tively easy. There is a base of knowledge around SQL that has not yet been developed
around NoSQL, which is still quite young. It is safe to say that 10 years from now SQL
databases will look similar to the way they do today, while NoSQL still has a lot of
evolution left simply because it is a new product family.
In order to be fault tolerant, a database, like an application server, must be able to scale
to more than one computer and be able to handle the case where a server dies. In
addition, to be scalable, each server must be independent. If with three nodes a cluster
can serve N requests per minute, then with six nodes it should be able to serve 2N
requests per minute (or at least close). In reality this is not usually possible, as conten-
tion for shared resources will get in the way. True linear scaling is a theoretical best case.
CAP Theorem
The CAP theorem is an idea proposed by Eric Brewer that states that it is impossible
for a distributed computer system to provide strict guarantees on all three of Consis-
tency, Availability, and Partition Tolerance at the same time. This theorem has in fact
been mathematically proven to be true. A Google search will reveal the full details of
the proof for those who may be interested.
A consistent system is one in which all nodes see the same data at all times. This is
traditionally seen in single-node systems or those running on a small number of nodes.
Most SQL databases feature extensive features in terms of transactions and the like to
make sure that the data is always consistent at any given time, and in some cases this
is an important feature.
It is possible to achieve consistency on massively concurrent systems; however, it must
be done at the cost of fault tolerance or availability. In some cases the cost of achieving
this may be quite high. In addition, if all nodes must agree on the state of data, this can
making handling failures much harder as nodes can go offline.
The problem with a fully consistent system is that when scaling up to many nodes, the
communication overhead can get very high. Every node must agree on all aspects of
the state of the data at all times. This can make scaling systems difficult, as two-phase
commits cause more and more locks to spread through the system.
However, full consistency is often not as important as people think. In many web scale
applications, if some users see new data a few seconds after others, it does not matter
that much—for example, if I post a new auction to eBay it’s not terribly important if
some users don’t see it for a minute or two. On the other hand, in some banking systems
this will matter a great deal.
10 | Chapter 1: Building Scalable Systems with Erlang and REST
An available system is one in which all clients can always read and write data. Obvi-
ously, having a system with guarantees about availability is a good thing; however, it
is not possible to combine this with partition tolerance and constancy. If a system must
be fully constant in the face of a network split, it must disallow writes as it will have no
way to make sure the data is consistent across all nodes.
The best example of a partition-tolerant database is the DNS system. The DNS system
is pretty much always available, but it is possible that some of the servers may be split
from others at any given time, in which case they will serve up old data until the issue
is resolved. Thus all users on the net will always be able to use the DNS system, but
may not always see the same data for a given query.
The CAP theorem is mostly brought up in terms of databases, but in truth it applies to
any distributed computing system. For example, Git and Mercurial version control tend
to be AP systems, while CSV and Subversion tend to be CA systems. Systems like Git
and Mercurial also need to explicitly handle the case where two sets of changes have
to be merged.
In fact, the CAP theorem applies to many areas that might not be obvious. For example,
foreign exchange is a widely available system that is not always exactly consistent. The
price quotes in exchanges around the world will in general be similar, but may differ
by a little bit and since it takes time for a signal to travel between London and New
York, being 100% consistent would actually be impossible.
Erlang systems are by definition distributed, so CAP applies to not just the data store
but the system as a whole. Understanding this idea is key to building a successful ap-
plication in a distributed environment.
Mnesia
Mnesia is Erlang’s own database. It is a very fast data store designed to work well with
Erlang, and it has several nice advantages. It works with native Erlang records and code,
and it is also possible to set it up to serve data from RAM or from disk and to mirror
data across nodes. You can even set it up so that data lives in memory on most nodes
but is mirrored to disk on one or two nodes, so that all access is in memory for very
fast operations but everything is written out to disk for long-term persistence.
Technically the Mnesia data store is ETS and DETS. Mnesia is a trans-
action and distribution layer built on top of them.
The one possible problem with Mnesia is that while it is not a SQL database, it is a CA
database like a SQL database. It will not handle network partition. This does not mean
that it is not usable in scalable applications, but it will have many of the same issues as
SQL databases like MySQL.
New Opportunities for Scaling and Resilience | 11
Mnesia is built into Erlang so there is nothing to install. However, it must be started
when Yaws is started. To do this, use the OTP function application:start(mnesia).
to start up the Mnesia database. From here, tables can be created with the mnesia:cre
ate_table/2 function, which uses Erlang records as its table schema. For full details of
how to use Mnesia, see some of the Erlang references. The Erlang documentation also
includes a set of man pages on Mnesia.
By using the qlc module, it is also possible to treat a Mnesia table as if it were a big
array, so you can use Erlang’s array comprehensions to pull data out of Mnesia. It is
even possible to do things like foldl to summarize data in a table.
CouchDB
CouchDB is a data store that is actually written in Erlang. Unlike Mnesia and MySQL,
CouchDB is not organized around records with a fixed schema; rather, it’s a document
store that takes some ideas from Lotus Notes. In fact, Damien Katz, who created
CouchDB, used to work on Lotus Notes.
CouchDB also gives up strict consistency for an eventual consistency. By doing this, it
can create guarantees of partition tolerance and availability. In a CouchDB network
every node can be a master, and even if two nodes are not in communication, both can
be updated.
This lack of consistency has some costs, but it also has some major benefits. In many
cases, making sure all nodes agree about the state of data at all times is a very expensive
operation that can create a lot of load on a large system.
There are multiple interfaces from Erlang to CouchDB, including couchbeam, eCouch,
erlCouch, and erlang_couchdb. Each of these offers somewhat different features, but
several of them (including couchbeam and eCouch) run as OTP applications. Links to all
of these are available on the CouchDB wiki: http://wiki.apache.org/couchdb/Getting
_started_with_Erlang.
MongoDB
MongoDB is also a NoSQL database, but it is designed to assume a consistent database
with partition tolerance and the ability to share data easily. MongoDB can be accessed
from Erlang with the emongo driver available from https://bitbucket.org/rumataestor/
emongo. The API is quite straightforward and documented at the website.
Redis
Redis is also a key value data store, but unlike MongoDB and CouchDB, Redis normally
keeps its entire dataset in memory for very fast access, while keeping a journal of some
form on disk so that it is still persistent across server restarts. Like Mongo, it is a CP
data store.
12 | Chapter 1: Building Scalable Systems with Erlang and REST
There are two sets of drivers for Redis in Erlang, Erldis and Eredis, both of which can
be found on the Redis home page at http://redis.io.
Riak
Riak is yet another document database that is similar to CouchDB in some ways. Like
CouchDB, it is written in Erlang and gives up strict consistency for availability, scala-
bility, and partition tolerance. It is meant to be a distributed system and has good
support for scaling out by adding nodes~, and scaling back in by removing nodes that
are no longer needed. Riak can be found at http://www.basho.com.
Riak is derived in large part from Amazon’s Dynamo database. The idea is that you
split many nodes over a consistent hashing ring, and any key in the database gets sent
to the nodes taking charge of a given section of the ring.
The great thing about availability is that the nodes are split in a way that might allow
a quorum system. That is to say that in a system of N nodes, for a write to be successful
all the nodes must agree to the transaction. That is a fully consistent system with lower
availability. If only some subset (M) of the nodes need to agree, then only a subset of
the cluster has to be responsive for things to work.
By adjusting the ratio of M:N it is possible for a system to be tuned in terms of the level
of consistency versus availability desired. This tuning can be set on a per-query basis
so the system is very flexible.
As Riak is primarily written in Erlang, there is excellent support for interfacing Riak to
Erlang applications.
New Opportunities for Scaling and Resilience | 13
