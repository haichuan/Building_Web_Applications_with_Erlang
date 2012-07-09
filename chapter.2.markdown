CHAPTER 2
Getting Started with Yaws
Most developers who are moving from other web development environments to Erlang
and Yaws will have used other web servers such as Nginx or Apache. The Erlang Yaws
web server performs the same basic tasks, but the details of performing common actions
are often different.
Erlang is not only a language, but also a runtime system and something that looks a lot
like an application server. As such, Erlang and Yaws (or other web servers) will fill the
same role as Apache/PHP/MySQL and other components all in one system.
The major differences between Erlang/Yaws and Apache/PHP have a lot to do with
how Erlang tends to set things up. Erlang assumes that systems will be clustered, and
processes in Erlang are somewhat different from those used in many other systems.
If you’ve used Apache with mod_php, you may remember that each request is handled
by a process or thread (depending on how things are set up). The classic Common
Gateway Interface (CGI) would start a new process for every request. These threads
and processes are constructions of the OS and are relatively heavyweight objects. In
Erlang the processes are owned not by the OS, but by the language runtime.
When building an application with Apache and PHP, for each request the web server
must bring up a copy of the PHP interpreter and quite possibly recompile the various
bits of PHP code that are to be run. This is an expensive operation. By comparison, in
Yaws the Erlang code is probably already compiled and loaded, so in practice most of
the time all Yaws will need to do is call the correct function.
An Erlang process is much more lightweight than an OS thread. The time it takes to
start one, to send a message between them, or to context-switch them is much smaller
than it would be with threads in C or Java, for example. This has some definite impli-
cations on how applications are designed. While Java will tend to use thread pools, in
Erlang it is considered normal to just create a process for each client or socket because
they are so inexpensive to use.
15
As Erlang processes are so lightweight and can be started up so quickly, Yaws can also
create a new process for each request that comes in without any problem. This means
that Yaws can scale up very well and quite quickly.
Working with Yaws
If you’ve never worked with Yaws, you have a few things to get used to. Yaws naturally
sets up clusters, and it has its own way to create dynamic content and handle requests.
Overall, however, Yaws is pretty easy to work with, and it uses the Erlang REPL so you
can try code out at the command line.
Starting Yaws
Once Yaws is installed (see Appendix A) it must be started. To start Yaws at the Unix
command line, simply run yaws. In Windows there are several options for starting Yaws
from the Start menu, but the most common method is to open a DOS command win-
dow from the Start menu and do it from there.
There are a number of command-line switches that you can pass to Yaws. These let
you set the node name or other options. This can also be done via the .erlang file, which
Yaws will read when it first starts up. This file should contain valid Erlang code and
should live in the user’s home directory.
When Yaws is started it will print out a few lines of information that look similar to
Example 2-1 and then drop into the Erlang REPL. At this point Yaws is fully functional
and will serve any requests that you send it. It may take a second or two from when
you start the Yaws executable to when it is ready to serve content to users.
By default, Yaws will be set up to listen on port 8000 (Example 2-1 changes it to 8081
due to something else using that port). Normally we want to run a web server on port
80 for HTTP or port 443 for HTTPS; however, many Unix-type systems will not allow
nonroot users to bind to ports numbered below 1024. Clearly, running Erlang as root
is probably not a good idea, so we need a different solution to this. It would be possible
to run Yaws behind a catching proxy server that will map port 80 to a higher port.
Alternatively, you could use a number of methods to attach to a higher port. Various
ways of doing this are documented on the Yaws website at http://yaws.hyber.org/priv
bind.yaws; you will need to figure out which one works best for your setup.
The port that Yaws listens on is in a <server> block in the yaws.conf file.
Each virtual host can listen on a different port or IP address, but they
will all be able to access the same modules.
16 | Chapter 2: Getting Started with Yaws
Example 2-1. YAWS at startup
Eshell V5.8.3 (abort with ^G)
(yaws@sag)1>
=INFO REPORT==== 1-Feb-2012::11:32:16 ===
Yaws: Using config file yaws.conf
(yaws@sag)1>
=ERROR REPORT==== 1-Feb-2012::11:32:16 ===
'auth_log' global variable is deprecated and ignored. it is now a per-server variable
(yaws@sag)1> yaws:Add path "/usr/lib/yaws/custom/ebin"
(yaws@sag)1> yaws:Add path "/usr/local/lib/yaws/examples/ebin"
(yaws@sag)1> yaws:Running with id="default" (localinstall=false)
Running with debug checks turned on (slower server)
Logging to directory "/var/log/yaws"
(yaws@sag)1>
=INFO REPORT==== 1-Feb-2012::11:32:17 ===
Ctlfile : /home/zkessin/.yaws/yaws/default/CTL
(yaws@sag)1>
=INFO REPORT==== 1-Feb-2012::11:32:17 ===
Yaws: Listening to 0.0.0.0:8081 for <1> virtual servers:
- http://www:8081 under /home/zkessin/Writing/ErlangBook/yaws/DocRoot
(yaws@sag)1>
Unless you redirect them to a file, any logging commands sent by programs running in
Yaws will appear in the Yaws startup code. You can also compile modules and test code
here. In a system that needs to be kept running for a long period of time, it may be
useful to start up the Yaws command line inside the Unix program screen, which will
allow the session to be suspended and resumed later from a different computer. For
testing and development I often run Yaws inside an Emacs shell buffer, from which I
can easily copy and paste code from a scratch buffer to test things.
When you start up Yaws it reads a yaws.conf file. The default location of this file will
vary depending on how Yaws was set up, but it can also be specified by a command-
line switch. If you need to reload the yaws.conf file for some reason, you can do so by
calling yaws --hup.
Serving Static Files
While web applications are built around dynamically generated content, almost all of
them also have some static files that need to be served to clients. These will often be
HTML, CSS, JavaScript, images, and other media. Yaws is capable of serving up static
files, and as in Apache there is no special configuration needed: just place the files under
the doc root and Yaws will happily push them out to the browser. (If Yaws is embedded
inside a larger Erlang application, this may not be the case.)
A typical Yaws install will be spread over multiple nodes, so it is important that each
node in a cluster have an up-to-date copy of each file. There are several ways to do this.
If the cluster size is small (just a few nodes) then simply using rsync to copy files around
may be a good solution. In a larger system, using the system’s package manager along
with a tool like Puppet (http://puppetlabs.com) to distribute the files may make sense.
Working with Yaws | 17
It may also be possible to use a system like CouchDB to replicate resources around a
network.
Using the CGI Interface
While it is best to use Yaws to manage code written in Erlang, you may find cases where
using another language via the old-fashioned CGI interface still makes sense. Thank-
fully Yaws can do this quite well—simply configure the yaws.conf file to recognize files
ending in .cgi or .php for correct handling.
In order to run scripts from Yaws, the <server> block in the yaws.conf file must have
allowed_scripts set to include “php” or “cgi” as appropriate. The Yaws website has
full details.
In addition, the out/1 function can be set up to call a CGI function by invoking the
yaws_cgi:call_cgi/2 function, in the case where a CGI function should be called con-
ditionally or otherwise need special handling.
Compiling, Loading, and Running Code
When you launch Yaws from a terminal, it will present a command-line REPL, which
can be used to interact with Yaws and Erlang. This is a very easy way to play around
with Yaws and try things out.
There are several ways to compile and load Erlang code. In testing, the easiest way is
to type c(module). at the Yaws command line. This will compile the Erlang code down
to a .beam file, which is Erlang’s binary format, and load it into the Erlang system. Using
lc([module1, module2]). will do the same with a list of modules. In general,
the .beam files should be placed in directories in the code search path. In this case,
when an unknown module is required, it will be loaded automatically. To explicitly
load a .beam file compiled externally, l(module). will load the .beam file. (All of these
take an atom, which is the name of the module. Other options from the shell may be
found by running the help/0 function from the Yaws command line.)
Erlang programs run in a virtual machine, in the same way that Java
and .NET programs do. In Erlang’s case, the virtual machine was orig-
inally called “Bogdan’s Erlang Abstract Machine” and is now “Bjorn’s
Erlang Abstract Machine” (Bogdan and Bjorn being the programmers
who created them). As such, Erlang’s binary object files have the ex-
tension .beam.
You can also change directories and view the current directory contents by using the
cd/1 and ls/0 shell commands.
18 | Chapter 2: Getting Started with Yaws
Example 2-2 shows a simple interaction in the Erlang shell. The shell is opened, we
check the current directory with pwd/0, and then check the files in the directory with
ls/0.
Example 2-2. ls and pwd
Erlang R14B02 (erts-5.8.3) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:0] [kernel-
poll:false]
Eshell V5.8.3 (abort with ^G)
1> pwd().
/home/zkessin/Writing/ErlangBook/running
ok
2> ls().
.svn
example.erl
test.erl
ok
3> c(test).
{ok,test}
4> test:test(1234).
1234
5>
Then the test module, shown in Example 2-3, is compiled with the c/1 function. In this
case the module compiled correctly, so it returns {ok,test}. If there were errors they
would be reported here as well. Finally we run the test:test/1 function, which just
returns its arguments.
Example 2-3. test.erl
-module(test).
-export([test/1]).
test(X) ->
X.
The c/1 and l/1 functions will only load code on the current node. If you want to load
code on all connected nodes, use the nc/1 and nl/1 functions. These work just like the
single-node versions, but will propagate the changes out to all connected nodes.
The compile and load options mentioned above will also reload a module that is run-
ning. So it is easy to upgrade software; just reload it and make sure that functions are
called with an explicit module name to do an upgrade (this can happen with an explicit
message if desired).
In some cases—like if you’re doing something larger involving make—compiling from
the Yaws command line may not be the best choice. In that case there is an explicit
Erlang compiler erlc,1 which can be called from a Unix command line or from a build
utility such as Make, Ant, or Maven. The modules can be explicitly loaded from a Yaws
command-line switch or from the yaws.conf file. Normally an Erlang project is set up
1. The erlc executable and the command c/1 use the same code to do the actual compilation. Which one
to use mostly depends on which is better for the programmer.
Working with Yaws | 19
so that sources live in a src directory and the compiled files are moved to an ebin di-
rectory during the build process.
Erlang supports code autoloading. When a call is made to my_module:my_function/n, if
the module my_module is not loaded then Erlang will attempt to load the module.
When Erlang attempts to load a module, it will look in its file path in a very similar way
to how bash will find programs. You can see the contents of the Erlang path by running
code:get_path() from the Erlang REPL. This will produce a result similar to Exam-
ple 2-4. To add a new directory to the front of the path, call code:add_patha/1, and to
add one to the end call code:add_pathz/1. Both will return true if the call is successful
or {error, bad_directory} if not. Normally this should be done from an .erlang file in
your home directory.
Example 2-4. Erlang path (Truncated)
(yaws@sag)16> code:get_path().
["/usr/local/lib/yaws/ebin",".",
"/usr/lib/erlang/lib/kernel-2.14.3/ebin",
"/usr/lib/erlang/lib/stdlib-1.17.3/ebin",
"/usr/lib/erlang/lib/xmerl-1.2.8/ebin",
"/usr/lib/erlang/lib/wx-0.98.9",
"/usr/lib/erlang/lib/webtool-0.8.7/ebin",
"/usr/lib/erlang/lib/typer-0.9/ebin",
"/usr/lib/erlang/lib/tv-2.1.4.6/ebin",
"/usr/lib/erlang/lib/tools-2.6.6.3/ebin",
Clustering Yaws
One of the major benefits of Erlang (and by extension, Yaws) is the fact that it is de-
signed to exist in a clustered environment. The Yaws service itself can be clustered by
simply starting it up on multiple nodes and then putting a standard load balancer in
front of the web servers. However, in many cases the real power of Yaws will come
from clustering a few nodes running Yaws with a larger Erlang application. As Yaws is
native Erlang code, Yaws code can send and receive Erlang messages, which enables a
Yaws application to exist inside an Erlang ecosphere.
In order for hosts to communicate, they must share a cookie value that should be kept
secure. This cookie can be specified on the command line, set with an Erlang built-in
function (BIF), or set in the .erlang.cookie file. Erlang will create that file with a random
value if it is needed but not found. When setting up an Erlang network, finding a good
way to distribute this cookie file is probably a good idea.
When working across multiple nodes one must be careful that the same
code is always loaded on all nodes. Erlang has features to do that, such
as the shell command lc/1, but will not load a new module on every
node by default. While upgrading a system, the software must be able
to deal with the case that some nodes may be running a newer or older
version of the software.
20 | Chapter 2: Getting Started with Yaws
Figure 2-1. Cluster diagram
Setting up links between nodes in Erlang is actually quite easy. The first time a message
is sent from one node to another, they will be connected together. So calling
net_admin:ping/1 or sending any other message will connect two nodes.
One nice thing about Erlang’s processes is that when sending messages between them
it does not matter where each process is running. The code Pid ! message sends message
to the process Pid. Pid can be on the same computer, in a second Erlang process on the
same host, on a different computer, or even on a computer running in a data center
halfway around the world.
In Figure 2-1 there are two nodes—A and B; within those nodes, there are three pro-
cesses numbered 1, 2, and 3. Messages can be sent between them via the ! operator
(represented here with an arrow) regardless of where the two nodes are.
In general, setting up cross–data center connections between nodes
should use SSL tunneling, and may have a number of issues relating to
delays between nodes.
Dynamic Content in Yaws
If the desired result is to output a page of HTML or XML, there are several good ways
to go about this. If you give Yaws a file with the extension .yaws, it will look for any
blocks in that file with the tag <erl> and run the out/1 function that is found in that
block. This is similar to how PHP will invoke code inside of a <?php ?> tag and how
many other systems do templates. It is also possible to render HTML or XML with a
template system like “ErlyDTL” (see “ErlyDTL” on page 26).
Yaws will in fact compile these files down to an .erl file, which will live in the
$HOME/.yaws directory. If there is a syntax error the exact path will be given.
Dynamic Content in Yaws | 21
It is customary in Erlang to name a function with the name and parity.
So out/1 is the function named “out” that takes one parameter, in this
case a data structure that describes the request. The function out/2
would be a separate function that simply shares a name with out/1.
How Yaws Parses the File
When the browser requests a file with a .yaws extension, Yaws will read the file from
the disk and parse that file. Any parts that are pure HTML will be sent to the browser.
However, anything in an <erl> block will be handled separately. Yaws will take each
<erl> block and convert it into an Erlang module. Yaws will then compile the code and
cache it in memory until the .yaws file is changed. As such, Yaws will not have to
recompile the source except when the file is changed or first accessed.
Yaws will then call the function out/1 and insert the return value of that function into
the output stream. If there is an <erl> block without an out/1 function, Yaws will flag
it as an error.
If Yaws finds two or more <erl> blocks in a file, it will just convert each one into a
module and compile and run them individually.
It is also important to note that unlike PHP, Yaws will not send any output to the socket
until the entire page is processed. So it is possible to set a header from the bottom of
the page after some HTML output has already been generated if that is needed.
If you want to understand the full process of how Yaws does all this, read the Yaws
Internals Documentation at http://yaws.hyber.org/internals.yaws and the source code
in yaws_compile.erl.
The out/1 function is called with a parameter of an #arg{} record that is defined in the
yaws_api.hrl file (see Example 2-5). All the data that might be needed to figure out
details of the current HTTP request are here and can be used to determine what to do.
This is the definition of the #arg{} record from the Yaws sources. In any .yaws files this
will be automatically included; otherwise you will have to include it in the header of
your module.
Example 2-5. Structure of the #arg{} record
-record(arg, {
clisock,
client_ip_port,
headers,
req,
clidata,
server_path,
querydata,
appmoddata,
%%
%%
%%
%%
%%
%%
%%
%%
%%
%%
the socket leading to the peer client
{ClientIp, ClientPort} tuple
headers
request
The client data (as a binary in POST requests)
The normalized server path
(pre-querystring part of URI)
For URIs of the form ...?querydata
equiv of cgi QUERY_STRING
(deprecated - use pathinfo instead) the remainder
22 | Chapter 2: Getting Started with Yaws
%% of the path leading up to the query
%% Physical base location of data for this request
%% virtual directory e.g /myapp/ that the docroot
%% refers to.
fullpath,
%% full deep path to yaws file
cont,
%% Continuation for chunked multipart uploads
state,
%% State for use by users of the out/1 callback
pid,
%% pid of the yaws worker process
opaque,
%% useful to pass static data
appmod_prepath, %% (deprecated - use prepath instead) path in front
%%of: <appmod><appmoddata>
prepath,
%% Path prior to 'dynamic' segment of URI.
%% ie http://some.host/<prepath>/<script-point>/d/e
%% where <script-point> is an appmod mount point,
%% or .yaws,.php,.cgi,.fcgi etc script file.
pathinfo
%% Set to '/d/e' when calling c.yaws for the request
%% http://some.host/a/b/c.yaws/d/e
%% equiv of cgi PATH_INFO
}).
docroot,
docroot_mount,
In Example 2-6, the HTTP method is extracted from the #arg{} structure and then
returned to be rendered into HTML, as shown in Figure 2-2.
Example 2-6. Using ARG
<erl>
method(Arg) ->
Rec = Arg#arg.req,
Rec#http_request.method.
out(Arg) ->
{ehtml, f("Method: ~s" , [method(Arg)])}.
</erl>
Figure 2-2. Output of Example 2-6
It is also possible to define your actual logic in a set of modules that are compiled and
loaded normally into Erlang and then use a set of .yaws files to invoke those functions
Dynamic Content in Yaws | 23
from the Web. To do this, use a .yaws file like that shown in Example 2-7. This has an
out/1 function that simply calls my_module:some_func/1, which does the actual work.
This way the actual logic can be held in normal Erlang modules but without the com-
plexity of appmods (see Chapter 3). Just remember to export the needed functions from
the Erlang modules.
Example 2-7. Calling an external function
<erl>
out(Arg) ->
my_module:some_func(Arg).
</erl>
In Example 2-8, we use the yaws_api:parse_post/1 function to return a list of the op-
tions sent over HTTP via POST. There is also a function yaws_api:parse_query/1 that
will return data sent in the query string as in an HTTP GET operation.
Example 2-8. Displaying POST variables
<erl>
out(Arg) ->
{ehtml, f("~p", [yaws_api:parse_post(Arg)])}.
</erl>
There are a number of options for what out/1 can return. If it returns a tuple like {html,
"Hello World"}, the string will be inserted literally into the HTML document.
EHTML
Among the options for return types from out/1 is {ehtml, DATA}, where “ehtml” is a
Domain Specific Language (DSL) that allows you to map Erlang data structures onto
HTML elements. Each element of an EHTML data structure should look like {Tag,
Attributes, Content}, and of course the content can be further EHTML records so the
entire EHTML structure is recursive.
Example 2-9. EHTML example
{table, [],
{tr, [{class, "row"}],
[{td, [], "Hello World"}]}}
The EHTML shown in Example 2-9 will produce the HTML shown in Example 2-10.
EHTML can also be used to produce XML if needed for web services.
Example 2-10. EHTML example output
<table>
<tr class="row">
<td>Hello World</td></tr></table>
24 | Chapter 2: Getting Started with Yaws
In all .yaws pages, Yaws includes the function f/2, which is an alias for the function
io_lib:format/2. This function is similar to the C function sprintf() except that it uses
“~” instead of “%” for formatting, which is to say that it takes a formatted string and
a list of arguments and returns a formatted string. For full details of all the options, see
the Erlang manual page at http://www.erlang.org/doc/man/io_lib.html#format-2.
Headers and Redirects
There are times when a web application will wish to set one or more custom headers
to send back with the content of the request. To do this, return the tuple {header,
HeaderString}. For example, {header, "X-Server: Yaws"} will send back “X-Server:
Yaws” as a header.
To return HTML as well as multiple headers, just put the tuples in a list in the return
values. Example 2-11 will cause Yaws to return a response similar to Example 2-12.
Example 2-11. Headers and content
<erl>
out(Arg) ->
[{html, "Header with HTML"},
{header, "X-Server: Yaws"}].
</erl>
Example 2-12. Headers and content response
HTTP/1.1 200 OK
Server: Yaws 1.90
Date: Fri, 30 Dec 2011 08:50:32 GMT
Content-Type: text/html
X-Server: Yaws
Header with HTML
There are a few headers that are so common that Yaws provides a shorthand method
of sending them. You can set the headers connection, location, cache_control,
set_cookie, content_type, or content_length with the following format:
{content_length, 4312}; that is, as a simple pair of atom and value.
In addition, by returning the tuple {status, Code}, Yaws allows you to send back a
status other than “200 OK”. So it is possible to send back “201” for a resource created
or “405” if the user sent a request with an illegal method. To do this, return
{status, 201}.
To redirect the user to a different URI from the out/1 function, return the tuple {redi
rect, URL}. Yaws will send back a HTTP 302 Found response, which will cause the
browser to redirect to the new URI. See Example 2-13.
Dynamic Content in Yaws | 25
Example 2-13. Redirect
<erl>
out(Arg) ->
URL = "http://www.erlang.org",
{redirect, URL}.
</erl>
The HTTP standards require a full URL for requests (see Example 2-13). However, in
many cases the redirect may be from one resource on a server to another on the same
server, so using a relative URI may make sense. Fortunately Yaws provides a way to do
this by returning {redirect_local, RELATIVE_URI}, as in Example 2-14. Of course, in
both of these cases, the choice of whether to redirect as well as the location to redirect
to do not have to be fixed at compile time.
Example 2-14. Local redirect
<erl>
out(Arg) ->
RELATIVE_URI = "/some_other_file.yaws",
{redirect_local, RELATIVE_URI}.
</erl>
If in development you get stuck in redirect confusion, try using curl to
sort things out. It will allow you to see each redirect that the server sends
back and figure out what went wrong. To make curl redirect, pass it the
--location option.
Templates
In addition to EHTML and the f/2 function described in “Dynamic Content in
Yaws” on page 21, there are several template packages available on Erlang. These tem-
plate engines allow the developer to separate HTML from data processing, which is
always good practice. This is nice because it frees you from needing to have the structure
of the returned Erlang data match the exact structure that will be shown on screen to
a user. It also provides a powerful and well known set of transformations to convert
the output of the Erlang functions to the HTML that the user can see.
ErlyDTL
If you are familiar with the Python Django template library, you’ll want to check out
the ErlyDTL package. ErlyDTL is a port of the Django template library to Erlang, and
you can find it on GitHub at https://github.com/evanmiller/ErlyDTL. Full documenta-
tion for ErlyDTL can be found there, and the full documentation for the Django tem-
plate library can be found at the Django website: https://www.djangoproject.com/.
26 | Chapter 2: Getting Started with Yaws
The ErlyDTL compile/2 function takes a template—which can be a string that will be
interpreted as a path to a file or a literal template as a binary and a module name—and
convert it into a compiled Erlang module with a few defined function that can be used
to render the template and get some information about it. There is also a compile/3
function that allows the developer to specify options for the compilation.
To compile a template as in Example 2-15, first load the ErlyDTL package (line 2). In
this case it was necessary to first change Erlang’s search path with code:add_patha/1.
After that, in line 4, ErlyDTL:compile/2 compiles the templates.
Example 2-15. Compiling ErlyDTL templates
(yaws@sag)1> code:add_patha("<root>/templates/erlydtl/ebin").
true
(yaws@sag)2> l(erlydtl).
{module,erlydtl}
(yaws@sag)3> cd("templates").
/home/zkessin/Writing/ErlangBook/yaws
ok
(yaws@sag)4> erlydtl:compile("<root>/templates/hello-world.dtl", hello_world).
ok
Building ErlyDTL as Part of a Make Process
In Example 2-15, the template is compiled on the Erlang REPL, which is great for testing
things out and making sure that they work correctly. However, a real project will prob-
ably need to do something like continuous integration and will require a different sol-
ution to building ErlyDTL templates.
In this case, templates should be located in their own directory and compiled with the
script in Example 2-16 as part of the build process. The script will compile the templates
down to .beam files that can be loaded as any other module.
This script should be called like in Example 2-15 and can be called from Make, Emake,
or your build system of choice. As long as the .beam is in Erlang’s search path it will be
loaded when needed.
erlydtl_compile templates/hello_world.dtl hello_world ebin
Example 2-16. ErlyDTL compile script
#!/usr/bin/env escript
-export([main/1]).
main([File_Name, Module, BinDir]) ->
l(erlydtl),
erlydtl:compile(File_Name,
Module,
[{out_dir,BinDir}]).
This script uses escript, which is a method of doing shell scripts in Erlang. The full
details are beyond the scope of this book, but the condensed version is that when run,
the main/1 function is called with a list of parameters passed on the command line.
Templates | 27
The ErlyDTL template, when compiled, will appear to Erlang as a module that exports
a few functions. The most basic form is template:render/1, which will return {ok,
Content} or {error, Error}. There is also a template:render/2 version that allows some
customization of the template function. It is possible to pass a locale, a translation
function that will work on {% trans %} tags. For a full list of options, see the ErlyDTL
web page.
ErlyDTL will take a Django template and compile it down to an Erlang .beam file with
standard functions for the templates that can be used just like any other functions.
Django templates put symbols in double bracket escaping, as in Example 2-17.
Example 2-17. A simple DTL template
<h1>Hello {{ planet }}</h1>
After the template has been compiled, it can be called with the module:render/1 function
as in Example 2-18.
Example 2-18. Calling the template
<erl>
out(Arg) ->
{ok,HTML} = hello_world:render([{planet, "Earth"}]),
{html, HTML}.
</erl>
Note that the render/1 function returns {ok, HTML} while out/1 should return some-
thing like {html, HTML}, so it is necessary to unwrap the results of the render function
and rewrap them with a different atom. The server will return output like in Exam-
ple 2-19 to the browser.
Example 2-19. Template output
<h1>Hello Earth </h1>
When invoking a Django template, the data to be passed in is sent as a list of pairs of
the form {Key, Value} where the Key is normally an Erlang atom. So in Example 2-18
the passed value is [{planet, "Earth"}]. If there are multiple values, you can pass
them in as a list. In addition, the value does not need to be a simple string, but could
be a data structure of some form that will be processed by the template to produce a
list or some other content.
28 | Chapter 2: Getting Started with Yaws
Not Quite Working Right
When something goes wrong in a Yaws request, it will show a screen like Figure 2-3.
In this case, the template has malfunctioned.
hello_world:render/1 was not found because the module had not been loaded.
When ErlyDTL compiles a .dtl file, it will by default load the code into the Erlang Virtual
Machine but will not save a .beam file so that you have to specify the option out_dir as
part of the compile, which will tell ErlyDTL where to store the .beam files. If not speci-
fied, it will not create them.
Figure 2-3. Django template not quite working
Django templates can of course do more than just interpolate variables as in Exam-
ple 2-17. It is also possible to include display logic in your templates. For example, you
can have logic to iterate over a list by using {% for i in somelist %} and within that
to have alternate rows be styles by using logic like {% cycle odd even %}.2
You can also use ErlyDTL to build XML as well as HTML files, as shown in Exam-
ple 2-20.3 Here the template iterates over a list of articles and pulls data out of each
field in the #article record to build an XML document for an RSS feed.
2. It is also possible to have alternate styles on rows by using CSS selectors like :nth-child(even) and :nth-
child(odd), so there are multiple ways to do that. However, cycle can be used in other places and so
should not be discounted.
3. This is taken from Fred Hebert’s “blogerl” project. Fred was nice enough to allow me to use this example
here.
Templates | 29
Example 2-20. RSS template
<?xml version="1.0"?>
<rss version="2.0">
<channel>
<title>Ferd.ca</title>
<link>{{ url.base }}</link>
<description>My own blog about programming and whatnot.</description>
<language>en-us</language>
<pubDate>{{ latest_date }}</pubDate>
<lastBuildDate>{{ latest_date }}</lastBuildDate>
<ttl>60</ttl>
{% for article in articles %}
<item>
<title>{{ article.title }}</title>
<link>{{ url.base }}{{ article.slug }}.html</link>
<description>{{ article.desc }}</description>
<pubDate>{{ article.date }}</pubDate>
<guid>{{ url.base }}{{ article.slug }}.html</guid>
</item>
{% endfor %}
</channel>
</rss>
A full tutorial on the Django template library is beyond the scope of this book, but it
can be found at https://docs.djangoproject.com/en/dev/ref/templates/builtins/. This docu-
ments all of the various forms that can be used in DTL.
There are other Erlang template engines floating around the Web be-
sides ErlyDTL. Each has its own strong and weak points, so you’ll want
to look around for the best solution to a specific problem.
Logging
Of course any web server needs to be able to log data, which can be used to analyze
errors as well as for reasons like business intelligence.
Yaws will create a number of log files by default. Where these files live can be configured
in the yaws.conf file, but normally they will be somewhere like /var/log/yaws.
The first log is the access log, which is presented in the same format that Apache and
a number of other web servers save their log files. See Example 2-21. (Note that this
would normally be all on one line but has been wrapped over several to fit the page in
this book.)
30 | Chapter 2: Getting Started with Yaws
Example 2-21. Access log
127.0.0.1 - - [24/Feb/2012:11:31:02 +0200] "GET /templates/hello-world.yaws HTTP/1.1" 500
774
"-" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.7 (KHTML, like Gecko)
Ubuntu/11.10 Chromium/16.0.912.77 Chrome/16.0.912.77 Safari/535.7"
Also, any errors that are reported in Yaws will be reported in the file /var/log/yaws/
report.log in a format that looks somewhat like Example 2-22.
Example 2-22. Report log
=INFO REPORT==== 20-Sep-2011::13:49:39 ===
Yaws: Listening to 0.0.0.0:8080 for <1> virtual servers:
- http://localhost:8080 under /usr/share/yaws
=ERROR REPORT==== 24-Sep-2011::19:15:26 ===
Yaws: bad conf: Expect directory at line 130 (docroot: /var/www/yaws) terminating
As we have seen in prior examples, if you call io:format/1 it will send the string to the
Yaws console. This is useful for testing and such but is not captured for later use. What
is really needed is a package that will log messages to a file on disk or other central
point for later collection and analysis.
Erlang OTP error_logger
Erlang provides a standard error_logger package as part of OTP that allows the pro-
grammer to send the normal info, warning, and error levels.
Erlang processes direct their console IO to the console of their group
leader. By default, this will be the process that started up that process
(or recursively up to a console); however, it is possible to change this.
See the Erlang manual for more information.
The error logger is a standard part of the Erlang/OTP kernel and will always be present.
By default it will send errors to the tty; however, by setting the error_logger parameter
to {file, FileName} it will send errors to the named file. It is also possible to set the
value of error_logger to silent to turn off error reporting altogether.
To send an error message call error_logger:error_msg/1 or error_logger:error_msg/
2. The first version takes a literal string that is sent to the error message as is. The second
version takes a string and a list of parameters that will be interpolated into that string.
The formats for this are the same as in io:format/2, which resembles C’s “sprintf” but
with the percent signs replaced by tildes.
Demonstrating the use of logs in an Erlang program, Example 2-23 shows a basic info
log statement in a .yaws file.
Logging | 31
Example 2-23. Code with logging
<erl>
out(Arg) ->
error_logger:info_msg("~p:~p User Entered the System~n",
[?MODULE,?LINE]),
{html, "Logger"}.
</erl>
It will produce output like in Example 2-24 on the console or in a file, depending on
how the system is set up.
Example 2-24. Log message
=INFO REPORT==== 16-Jan-2012::13:38:52 ===
m1:13 User Entered the System
The macros ?MODULE and ?LINE expand to the current module and the current line in
that file. So by putting them in the log statement it is possible to see where the log
message was generated. As it was from a .yaws file in this example, the module will
resolve to something like m1 and the line will not be the actual line of the .yaws file.
The functions error_logger:warning_msg/1,2 and error_logger:info_msg/1,2 work
exactly the same as the error_msg/1,2 functions, but for a different level of errors.
32 | Chapter 2: Getting Started with Yaws
