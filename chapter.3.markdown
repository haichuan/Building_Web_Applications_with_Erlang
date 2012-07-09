CHAPTER 3
Appmods: Dynamic Content in Yaws
When developing a web service, there are times when the developer does not want to
map the URL that is sent by the web browser to an actual file. This may be because all
data is stored in a database or is generated dynamically. In this case you want the
browser to go to some URL and have the server create the content and return it.
Appmods are to Yaws what Apache modules are to Apache—a way to create a custom
handler for a URL in the web server. When using an appmod, the browser will send a
standard URL, but instead of having a static file sent back to the user, Yaws will execute
some Erlang code and send the response back to the user. This allows for fully dynamic
content in the Yaws web server.
The difference is that in the case of an appmod, out/1 will need to create the full content
of the response, while in a .yaws file it may just be a small part of it. In many cases, the
response will not be HTML but will be some other format, such as XML
(“XML” on page 49), JSON (“JSON” on page 47), or even something like an audio
file, CSV text file, or PDF.
In addition, by using an appmod you can break the association between a file on the
local disk of a node and the URL representation that is presented. So it is possible to
present a URL like /blog-posts/2011-Dec-02/why-you-should-use-erlang.html without
there actually being a file at that path (or even a directory structure)—just some code
that knows how to map that URL onto some logical resource that can then be con-
structed. Similar effects can be achieved with mod_rewrite in Apache or with other
Apache modules.
In the case of a binary format, then, Erlang should create the data and return it with
the correct header. The specifics of how to create a binary will depend on the format
of the data and are beyond the scope of this book. However, in many cases there will
be Erlang modules to create the data; in some cases the data will be streamed to the
client.
If an appmod is set up at the root of the web server, then with a request to http://
example-server.com/chatroom/example-room.json the Arg#arg.appmoddata field will be
33
set to /chatroom/example-room.js, which is the part of the path to be handled by the
appmod.
It is also possible to set up the appmod to handle only a subset of the web server, in
which case the path passed to the appmod will be just the end of the URI and will be
in Arg#arg.appmod_prepath.
Appmod Configuration
To set up an appmod, add an appmods field to the <server> block in yaws.conf. The
general format of this is shown in Example 3-1.
The basic form of the appmods configuration is <path, module>. The path can be any
path on the web server. If you want the appmod, serve all content from the web server,
set the path to “/”, which will route all requests to the appmod. However, even in this
case you’ll probably want some directories to be served as static files, (for example, the
images directory), so it is possible to add a list of paths to be excluded with the
exclude_paths directive, as in Example 3-1.
Example 3-1. Appmod config options
appmods = <path, module exclude_paths icons css js>
In this case, any path called that was not in the icons, css, or js directories will be routed
to the function module:out/1. Here the out/1 function works the same as in “Dynamic
Content in Yaws” on page 21.
When the URI Does Not Correspond to a File
In the case of .yaws files, the HTTP path sent to the server will map directly onto the
file. The user will make a request like “/get-stock-price.yaws” and Yaws will invoke the
code in the file get-stock-price.yaws.
However, in an appmod the programmer has to translate the request URI into some
action directly. This transfers some workload from the web server to the developer, but
it is not overly hard to do.
In order to do this we need to find out what URI the user requested from within our
handler function. This can be set in one of several places in the #arg record. It will be
in pathinfo or in fullpath (actually both).
In general, the path info will be a set of strings separated by slashes, so a request to
/record.yaws/show/3141/5926 will have pathinfo set to show/3141/5926. This string
should be split with re:split/3 and then used to show the correct data.
This path can be split up into individual tokens by using re:split/2 or string:tokens/
2. As shown in Example 3-2, both of these will take the string and a token or regular
expression to split up the string and return a list. However, there is a slight difference.
34 | Chapter 3: Appmods: Dynamic Content in Yaws
The re:split/2 function will return a list of binaries and will leave the empty string at
the start of the list. On the other hand, string:tokens/2 will return a list of strings and
will not include the initial blank element.
Example 3-2. Splitting a string
1> T = "/show/3141/5926".
"/show/3141/5926"
2> re:split(T, "/").
[<<>>,<<"show">>,<<"3141">>,<<"5926">>]
3> string:tokens(T, "/").
["show","3141","5926"]
In Example 3-3, the path/1 function splits the appmoddata path from the arg record on
the directory separator character (“/”) and then uses a case statement to match the path
against various options that will provide correct handlers depending on details of what
is passed. The patterns can match specific strings or have array elements assigned to
variables. Patterns will match from top to bottom until one matches or no pattern
matches, which will case the process to crash. A full description of pattern matching is
beyond the scope of this book, but the concept is very important for programming in
Erlang.
By adding the term [{return,list}, trim] to the re:split/3 function, it will drop
any empty elements, and return the result as a list of strings and not in a binary format.
Example 3-3. Path
path(Path) ->
Elements = re:split(Path,"/", [{return, list}, trim]),
case Elements of
["ChatRooms", Room] ->
act_on_room(Room);
[Directory, File] ->
show_file(Directory, File);
[Directory] ->
show_directory(Directory)
end.
Cookies
When the World Wide Web was first created back in the 1990s, each HTTP request
was independent and web requests had no ability to maintain any form of state. So for
example, a web server had no easy way to keep track of items in a user’s shopping cart.
Netscape introduced cookies in an early version of Navigator and they are now standard
in all modern browsers. A cookie can be set by the browser or the server, and once set
will be sent in the headers of all HTTP requests to that server until it expires or is
removed. Yaws can of course set cookies and access them.
Cookies | 35
In general, cookies are used to track the activities of a user on a website. This can include
authentication as well as state. For example, if the site implements a shopping cart, the
user’s current items can be tracked by a cookie. It is usually best to not put the items
themselves in the cookie, but to put a hash that can refer to a database record that
contains the cart information. This will greatly reduce the bandwidth used by the
application.
HTTP cookies are a form of persistent data that can be set by the browser or the server
and will accompany every request sent to the server. To set a cookie, use yaws_api:set
cookie/2, which takes the cookie name and value. If you want to set options for the
cookie, check out the yaws_api man page, that has versions of yaws_api:setcookie/n
which take extra parameters to allow you to specify a bunch of other options.
Do not confuse the HTTP cookies discussed here with the cookies that
Erlang uses to provide security when connecting nodes.
You can also set a cookie by having out/1 return {set_cookie, Cookie}, since cookies
are part of the HTTP headers.
To get the existing cookies, look at the headers record of the Arg record. The function
yaws_api:find_cookie_val/2 can extract the value of a cookie from the list of cookies,
as in Example 3-4. If a cookie is not set, this function will return empty HTML.
Example 3-4. Cookies
<erl>
out(Arg) ->
Headers = Arg#arg.headers,
Cookie = Headers#headers.cookie,
Prefs = yaws_api:find_cookie_val("Prefs", Cookie),
{html, Prefs}.
</erl>
Yaws also includes a set of interfaces to create session tracking with cookies (see
“Session Handling” on page 36).
Session Handling
Yaws provides a nice API for handling sessions with cookies with the
yaws_api:new_cookie_session/1-3 functions. The basic function new_cookie_session/
1 takes a state record that can be specified by the application. This record can be re-
trieved by the function yaws_api:cookieval_to_opaque/1.
To update the session data, use the function yaws_api:replace_cookie_session/2 with
the name of the cookie and the new state value.
36 | Chapter 3: Appmods: Dynamic Content in Yaws
In addition to the new_cookie_session/1 function, there is also a new_cookie_session/
2 that takes a timeout (TTL) value after which the cookie session will be cleared. In
new_cookie_session/1 the session will time out after a default period of time.
If some form of cleanup after a session ends is desired, use the new_cookie_session/3
function. In addition to a state variable and a TTL, this function also takes a PID for a
cleanup process. When a session ends, it will send that process a message of the form
{yaws_session_end, Reason, Cookie, Opaque}. Reason can be timeout or normal.
To remove a session, use the delete_cookie_session/1 function, which will remove the
cookie and send a cleanup message if needed.
In Example 3-5, which is taken from the Yaws sources, there is an example of session
handling. Similar to the way PHP treats the $_SESSION construct, Yaws does not actually
save the record to the HTTP cookie but will instead store a key of the form non
ode@nohost-5560960749617266689 and store the cookie on the server. Normally the
cookie and data will be stored in the Yaws processes; however, you can set it to store
in a Mnesia or ETS data store. There are examples for this at http://yaws.hyber.org/
pcookie.yaws.
Example 3-5. Cookie session handling (session.erl)
-record(myopaque, {udata,
times = 0,
foobar}).
out(A) ->
H = A#arg.headers,
C = H#headers.cookie,
case yaws_api:find_cookie_val("baz", C) of
[] ->
M = #myopaque{},
Cookie = yaws_api:new_cookie_session(M),
Data = {ehtml,
{html,[],
["I just set your cookie to ", Cookie, "Click ",
{a, [{href,"session1.yaws"}], " here "},
"to revisit"]}},
CO = yaws_api:setcookie("baz",Cookie,"/"),
[Data, CO];
Cookie ->
case yaws_api:cookieval_to_opaque(Cookie) of
{ok, OP} ->
OP2 = OP#myopaque{times = OP#myopaque.times + 1},
yaws_api:replace_cookie_session(Cookie, OP2),
Data = {ehtml,
{html,[],
[
"Click ",
{a, [{href,"session1.yaws"}], " here "},
"to revisit",
{p, [], f("You have been here ~p times",
[OP2#myopaque.times])},
Session Handling | 37
end.
end
{p, [], f("Your cookie is ~s", [Cookie])}]}},
Data;
{error, no_session} ->
new_session()
new_session() ->
M = #myopaque{},
Cookie = yaws_api:new_cookie_session(M),
Data = {ehtml,
{html,[],
["I just set your cookie to ", Cookie, "Click ",
{a, [{href,"session1.yaws"}], " here "},
"to revisit"]}},
CO = yaws_api:setcookie("baz",Cookie,"/"),
[Data, CO].
Access Control
Sometimes you may wish to to restrict access to resources, for example to users who
have entered a password or can otherwise be authenticated (as in a Facebook applica-
tion). In many cases you may wish to do something like check a username and password
or session token against a Mnesia database or other data store.
Ideally you would validate the username and password against some source of data,
such as a Mnesia table. In Example 3-6, I use the function validate_username_password/
1 that extracts the username and password from the request and checks them against
the Mnesia table. This function will return either {true, Uuid} if the user authenticates
correctly, or {false, Reason}. In this case, Reason can be no_user in the case where there
is no user by that name, or bad_password. Clearly sharing the reason why the login was
rejected would be a bad idea.
The out/2 function takes the result of validate_username_password/1 and returns either
{status, 401} if the user did not authenticate or a HTML page. It also logs the login
attempt.
Example 3-6. Custom access control (access-control.erl)
-module('access-control').
-include("../roulette/yaws_api.hrl").
-export([out/1]).
-record(user,
{
uuid,
username,
passwordMD5
}).
38 | Chapter 3: Appmods: Dynamic Content in Yaws
validate_username_password(Arg) ->
Username = yaws_api:queryvar(Arg, "Username"),
Password = yaws_api:queryvar(Arg, "Password"),
PasswordMD5 = crypto:md5(Password),
Query = fun () ->
mnesia:read({username, Username})
end,
Value = mnesia:transaction(Query),
case Value of
{atomic, []} ->
{false, no_user};
{atomic, [UserRecord]}
when UserRecord#user.passwordMD5 =:= PasswordMD5 ->
{true, UserRecord#user.uuid};
{atomic, [_UserRecord]} ->
{false, bad_password}
end.
out({false, Reason}, _Arg) ->
io:format("~p:~p Unable to login user: ~p", [?MODULE, ?LINE, Reason]),
{status, 401};
out({true, Uuid}, _Arg) ->
io:format("~p:~p Welcome: ~p", [?MODULE, ?LINE, Uuid]),
{html, "Hello World"}.
out(Arg) ->
out(validate_username_password(Arg), Arg).
Interacting with Erlang Services and Business Logic Layers
In many cases a web service will be a simple wrapper around a more complex middle
layer made up of OTP servers, which can be communicated with by Erlang’s standard
message passing methods.
To do this, Yaws must be clustered with the other Erlang nodes and must know the
PID of the process to communicate with. Example 3-7 shows how to make this work.
Example 3-7. Interacting with middleware
out(Arg) ->
BackendPid ! {webrequest, node(), Arg}
receive
{response, Data} ->
Data;
{error, ErrorMsg} ->
ErrorMsg
after 500 ->
[
{status, 404},
{html, "<h2>System timed out</h2>"}]
end.
Interacting with Erlang Services and Business Logic Layers | 39
There are a few things here to note. First of all, sending messages never fails, but there
is no guarantee of delivery. So if the process has gone away for some reason, the send
will not return any sort of error. Therefore the code in Example 3-7 must have a timeout
to let the user know that something went wrong. In this case it is the after clause, which
will wait 500ms and then return a timeout error.
A better way to handle this would be to wrap the middleware in a gen_server and use
the OTP framework to create a number of custom servers to run the application. This
is done in Chapter 9. In this case, each module will export a set of access functions that
can be called and will use the OTP gen_server:call/2 or gen_server:cast/2 functions
to access the server infrastructure. The implementation of gen_server takes care of all
of the message passing so the ! operator is never explicitly used.
Compare Example 3-7 to Example 3-8. In the former, all the nonfunctional parts have
been hidden by gen_server:call/2, which is well-tested and can be assumed to work
correctly. In the latter, our out/1 function does not need to know anything about the
operation of the layer it is calling; it just calls the get_data/1 function, which serves as
an interface to some form of backend service.
Example 3-8. Interacting with a service via OTP
get_data(Req) ->
{response, Data} = gen_server:call(?MODULE, Req),
Data.
40 | Chapter 3: Appmods: Dynamic Content in Yaws
