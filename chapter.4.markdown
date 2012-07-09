CHAPTER 4
Implementing REST
So far this book has focused on the parts of how to build a web service, such as how to
use a template and respond to a request. It is now time to expand our view to building
larger, more complex integrated services to deal with complex business requirements.
In this chapter I will show how to build a simple RESTful service to store a list of
airports. The service will use GET to request a list of airports, POST to add a new
airport, PUT to update an airport, and DELETE to remove one.
This chapter will focus on the technical aspects of how to take an HTTP request in to
some Erlang code and turn it into the correct output data or server actions. So, for
example, the user may send an HTTP POST to create a new airport record, and the
code will then store that data in a Mnesia database.
Most of what is in this chapter also applies to any form of web services
insofar as they require the server to examine the incoming request and
make an appropriate response.
Decoding a Request
When an HTTP request is made to the server there are a number of pieces of data that
come with that request. All of these are sent to the out/1 function via the Arg data
structure, and they can be extracted from that data structure. In some cases there are
preexisting functions to extract the data, in others you will have to create functions to
extract what you need.
Extracting the User’s Request
It is important to understand how a web browser or other client sends data to the server.
In the case of a GET or HEAD request, data is sent via the URL and query string, so a
request could look something like get-stock-price?stock=ibm. There are two pieces of
information here: the first is the path of the command get-stock-price, and the second
41
is the query string stock=ibm. For those of you who are familiar with PHP this would
be delivered by the $_GET variable. In a POST or PUT request, in addition to the query
string, data can also be sent in the body of the request.
We could also make this request as /get-stock-price/ibm, which has
the advantage of the fact that there is no query string (the bit after the
“?”), and most implementations of the HTTP standard, including proxy
servers and browser, do not cache GET requests that have a query string.
We saw how to deal with this type of request in “When the URI Does
Not Correspond to a File” on page 34.
For values sent via GET or POST there are simple functions to extract data. The func-
tions parse_query/1 and parse_post/1 take the Arg data record and return a list of tuples
of the form [{Key, Value}]. So if the request URL ends with ...?record=31415926, then
parse_query/1 will return {"record", "31415926"}.
If instead of getting the entire list of parameters the code only cares about a specific
value, use the yaws_api:postvar/2 or yaws_api:queryvar/2 functions. These functions
will be imported automatically in all “.yaws” pages, and so can be used without the
yaws_api: prefix. These functions will return {ok, Value} if the variable was set or
undefined if it was not.
The yaws_api:getvar/2 function will call postvar/2 if the HTTP request is a HTTP
POST and queryvar/2 if the request was a HTTP GET.
In some cases (including the upcoming Example 4-15) the data is sent to the server not
as a set of name value pairs as from a HTML form, but as a JSON or XML object in the
payload of a HTTP POST request. In this case the user data is in the clidata field of
the #arg record. To extract this use code like in Example 4-1. This function takes the
Arg#arg.clidata field and decodes the JSON into a data structure. It then logs the data,
and finally it uses the rfc4627:get_field/3 function to extract a specific field from the
data structure. (This was extracted from Example 4-14.)
Example 4-1. Getting a JSON from a HTTP POST
out(Arg) ->
{ok, Json, _} = rfc4627:decode(Arg#arg.clidata),
io:format("~n~p:~p POST request ~p~n",
[?MODULE, ?LINE, Json]),
Airport
= rfc4627:get_field(Json, "airport", <<>>),
Airport.
If the user has uploaded a file with the mime type multipart/form-data, use the function
yaws_api:parse_multipart_post/1. For more information, see Chapter 5.
42 | Chapter 4: Implementing REST
Response and Headers
Another important part of REST is that HTTP status codes are used to return infor-
mation to the client. So when creating a new airport record we should return a status
of 201 Created, not 200 OK, and when a request is not successful because a resource
does not exist the service should return 404 Not Found. A complete list of HTTP status
codes can be found at http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html.
By default Yaws will return a status of 200 OK, so if some other status code is desired
have the out/1 function return {status, Code} with the desired HTTP code. The out/
1 function can return a list of tuples, so it is possible to set a status code, headers, and
content (and any other options) from the return code. Table 4-1 shows a list of selected
status codes.
Table 4-1. Selected status codes
Status Code Description
100 Continue Used when the client wants to send a large request; allows the server to accept or reject
            request based only on the headers. Client must send Expect: 100-continue.
200 OK Standard HTTP response. The body should contain content.
201 Created A new resource has been created.
202 Accepted Request accepted, but not yet acted on.
203 Non Authoritative Information Server processed the request but may lack a full response.
204 No Content The server processed the request but is not returning any content.
205 Reset Content Like 204, but the client must refresh its data.
206 Partial Content The server is sending only a part of the data. This can be used to resume an interrupted
                   download.
300 Multiple Choices Server is presenting data in several formats; the client should choose one.
301 Moved Permanently Redirect to new URI.
302 Found Originally “Moved Temporarily”; should not be used in favor of 303 and 307.
303 See Other Short-term redirection to a new URI.
304 Not Modified Indicates that the client should use a cached copy of the resource.
307 Temporary Redirect The resource is at a different URI on a temporary basis.
400 Bad Request Request cannot be fulfilled due to bad syntax.
401 Unauthorized The user must authenticate; this will prompt most browsers to ask for a username and
                password.
403 Forbidden The server refused to respond to a request.
404 Not Found Resource does not exist.
405 Method Not Allowed Request used a HTTP verb not supported by a particular URI.
406 Not Acceptable Server cannot generate content that matches the “Accept” headers. For example, an
                      image may be available only as a .gif and the client wants it as a .png.
Response and Headers | 43
Status Code Description
408 Request Timeout Server timed out waiting for the client to send the request.
409 Conflict Server cannot update resource due to a conflict, for example two users trying to update
            the same record.
410 Gone Resource has been deleted and will not return. Ideally should be removed from search
        indexes, etc.
411 Length Required Request must include the length of its content.
412 Precondition Failed Request does not meet some precondition.
413 Request Entity Too Large Could be used when a file to be uploaded is greater than the server wants to accept.
414 Request URI Too Long The client sent a request URI that was too long.
417 Expectation Failed Client sent an Expect request-header that the server cannot accept.
418 I’m a Little Teapot Short and stout.
429 Too Many Requests Used when one user is sending too many requests in a period of time.
500 Internal Server Error Generic error message.
501 Not Implemented Server cannot respond to the request method.
503 Service Unavailable Server temporarily unavailable.
There are many options for how to build a frontend for testing. Obviously we could
build a JavaScript application in a browser with jQuery and backbone.js or ExtJS.
However, for testing we will just use the Unix curl binary, which allows us to issue
commands from a command line or script.
To demonstrate this we will create a simple database listing airports. For each airport
we will store a number of pieces of information including the airport name, the
iata_code (e.g., “JFK”), the city and country where the airport is located, and a list of
runways. The runways are stored in a runway record. These records are defined in
Example 4-2.
Example 4-2. Airport record
-record(airport,
{code, city, country, name }).
In any application where there is persistent data, a choice must be made as to how to
store it. For this example we will use Erlang’s built-in Mnesia data store. Mnesia is
integrated with Erlang, so it will always be present when Erlang is present. It is also
quite powerful and can do things like partition data across multiple servers and much
more.
Mnesia is a rather flexible data store that mostly mirrors SQL features but is built into
Erlang. However, Mnesia does not have the kind of constraints built into SQL nor the
typing that SQL systems have. Mnesia also can be spread across several nodes. Mnesia
44 | Chapter 4: Implementing REST
tables can exist on disk or only in memory, which allows a lot of control over
performance.
To find the HTTP method that was used, look in the Arg data structure (see Exam-
ple 4-3). In this case we find the request structure Rec, and from there we look in the
method field. This could in fact be done in one line, but is shown in two for clarity.
Example 4-3. Deriving the method
method(Arg) ->
Rec = Arg#arg.req,
Rec#http_request.method.
Building the Response
When a request comes into the rest module it is routed to the out/1 function. This
function uses the method/1 function (Example 4-3) to find the HTTP method, and then
routes things to the handle/2 function. There are four versions of this function, one
each for GET, POST, PUT, and DELETE. Erlang will match the parameter and call the
correct function.
The HTTP verb GET, POST, HEAD, etc., is set as an Erlang atom and
not a string.
QLC stands for Query List Comprehension and is a set of macros that overload the
meaning of list comprehensions in Erlang to allow them to be used as a Mnesia database
query. 1 The general structure is [ReturnedValue || Row <- mnesia:table(TableName),
filters], so in the GET clause of Example 4-4 it is taking a list of all the records in the
table “airport”. This is similar to the SQL statement SELECT * FROM airports.
The code in Example 4-4 (taken from Example 4-14) shows how to use QLC to query
the Mnesia data store, and then turn that data into a JSON of the form seen in Exam-
ple 4-5, which can be sent to the browser. (How to create a JSON will be covered in
“JSON” on page 47.)
Example 4-4. Generating the content
do(Q)->
F = fun() ->
qlc:e(Q)
end,
{atomic, Value} = mnesia:transaction(F),
Value.
1. For those who have worked in .NET, this is similar to LINQ.
Building the Response | 45
convert_to_json(Lines) ->
Data = [{obj,
[{airport, Line#airport.code},
{city,
Line#airport.city},
{country, Line#airport.country},
{name,
Line#airport.name}]}
|| Line <- Lines],
JsonData = {obj, [{data, Data}]},
rfc4627:encode(JsonData).
handle('GET', _Arg) ->
io:format("~n ~p:~p GET Request ~n", [?MODULE, ?LINE]),
Records = do(qlc:q([X || X <- mnesia:table(airport)])),
Json = convert_to_json( Records),
io:format("~n ~p:~p GET Request Response ~p ~n", [?MODULE, ?LINE, Json]),
{html, Json};
Example 4-5. Generating the content (pretty printed)
{
}
"data": [
{
"airport": "BOS",
"city": "Boston",
"country": "US",
"name": "Logan"
},
{
"airport": "JFK",
"city": "New York",
"country": "US",
"name": "John F Kennedy"
}
]
In the case of the GET request we want to query the Mnesia database for all airports.
(This is a limited example; obviously in a real application this would probably be filtered
in some way.)
The GET clause of the handle/2 method calls the qlc:q function with a list compre-
hension that allows the function to retrieve the entire “airports” table. It would also be
possible to filter this using guards if needed. This will return a list of records which is
put into “Rec”.
In many cases the format in which we want to return data to the client may be specified
by the client. This could be done by using the HTTP Accept header. For example, an
application could send an Accept header like the following:
Accept: application/xml, application/json
This client would like a response in XML or JSON format, but would probably prefer
XML. Other clients may specify something else. In the case where a web service is being
46 | Chapter 4: Implementing REST
used to feed a JavaScript user interface, it is probably OK to ignore this and always
return one data format. However, more and more web services are being used for
computer-to-computer applications, and in this case it may be that being able to sup-
port multiple data formats is a key feature of an application design. It is also a good
idea to return the content to the browser or other client with the correct MIME type.
The choice of whether to allow multiple response formats will come down to the
specifics of what is required of an application. However, in most cases picking one and
sticking with it will be acceptable.
When choosing a response type, there are two possible ways that the code can decide.
If the server would rather send one format it can query the headers out of the
Arg#arg.headers data structure with a query that asks if a given format is allowed. One
could imagine a function like Example 4-6 where a MIME type and Arg are passed in
and it returns true or false if the MIME type is in the list. If the Allowed header is not
present, the program should do something well defined. It should also be able to deal
with a request that includes a format of */*, which indicates all formats are OK.
Example 4-6. Format allowed
requested_formats(Arg) ->
Rec
= Arg#arg.headers,
Accept = Rec#headers.accept,
[AcceptFormats| _] = string:tokens(Accept, ";"),
string:tokens(AcceptFormats, ",").
accept_format(Format, Headers) ->
Res = lists:any(fun (F) ->
string:equal(Format, F)
end, Headers).
JSON
One very common method of data exchange is JSON, which was created by Douglas
Crockford from the Object Literal Syntax of JavaScript and is defined by RFC 4627.
JSON is simple to read and there are JSON implementations for almost any language
that may be needed, so it plays well with others.
Once Mnesia has given us a list of airports, we must convert that data to JSON format
to transmit to the browser. To do this there are a number of Erlang modules that can
be used to convert Erlang data to a JSON representation. These include the rfc4627
module that can be found on GitHub, the json2 module that is included with Yaws,
and a bunch of others.
When decoding a JSON with the rfc4627:decode/1 function, there are two options.
The first is that it will return {ok, Result, Remainder}. In this case, Result is the decoded
JSON and Remainder is any part of the input string that was not parsed. If for some
Building the Response | 47
reason rfc4627:decode/1 cannot parse the JSON, it will return {error, Reason}. The
most probable cause of this is a malformed JSON.
If you are having problems with JSON format data, try passing it
through JSONLint (http://jsonlint.com). This will validate JSON strings
and pretty-print them as well.
Sometimes the client will send us a JSON; one problem here is that the name-value pair
format of a JavaScript object represented in a JSON does not map very well onto Er-
lang’s data structures. However, it is still possible to map a JSON object onto Erlang’s
data structures. Given the JSON in Example 4-7, the Erlang rfc4627 module will map
it onto a data structure as in Example 4-8.
Example 4-7. JSON object
{
}
"cust_id": 123,
"name": "Joe Armstrong",
"note": "wrote Erlang"
Example 4-8. Decoded JSON object
{obj,[{"cust_id",123},
{"name",<<"Joe Armstrong">>},
{"note",<<"wrote Erlang">>}]}
The mapping of JSON data types onto Erlang types is something to keep in mind.
Arrays in JSON map onto lists in Erlang. Numbers in JSON map onto numbers. String
values as shown in Example 4-8 are mapped onto binary values in Erlang. However,
there are a number of JSON encoders and decoders in Erlang, and not all of them will
map a JSON onto exactly the same data structure.
If you try to encode a PID value from Erlang into a JSON, it will not
work and will give a rather confusing error message.
The object is mapped onto a data structure starting with the atom obj to mark it as a
JSON object, then a set of name-value pairs as an array of two value tuples.
To get the value of a specific field from the JSON object use the rfc4627:get_field/2
function, which will take the data structure put out by decode/1 and the name of a field
as an atom and return the value of that field. So calling rfc4627:get_field(Obj,
name) on Example 4-8 will return <<"Joe Armstrong">>. In addition, there is a function
rfc4627:get_field/3 that works just like rfc4627:get_field/2 except that the third
parameter is a default value if the value is not set in the JSON.
48 | Chapter 4: Implementing REST
When constructing an obj structure as in Example 4-8, the function rfc4627:set_field/
3 will be helpful. It will take an object of the form shown in the example and return a
new object of the same type with a field set to a value. So calling rfc4627:set_field(Obj,
country, "Sweden") on the example record will add the country to the data structure.
To create a JSON string to pass to a client, use the rfc4627:encode/1 function, which
will take data in the same format put out by rfc4627:decode/1 and turn it back into a
JSON data string. So the data structure in Example 4-8 will be encoded into a JSON
that is equivalent to Example 4-7. The example here has been reformatted by JSONLint
to be easier to read; the output will be all on one line.
It would be tempting to try to use code similar to Example 4-9 to convert a generic
Erlang record to a JSON (or something that will be converted to a JSON). However,
the access to fields in a record must be done with literal atoms, so Rec#Type.Field won’t
work. It must be done as Rec#test_record.airport_name. (It is possible to use macros
here, however.)
Example 4-9. Convert to JSON (this won’t work!)
-module(convert_to_json).
-record(test_record, {room_name, room_users, desc}).
-export([convert_to_json/2]).
convert_to_json(Type, Rec) ->
Fields = record_info(fields, Type),
Struct = [{Field, Rec#Type.Field} || Field <- Fields],
{obj, Struct}.
XML
While our application uses JSON for data transfer, in some cases XML may be a better
choice. So having a way to convert data from Erlang records to XML would be a useful
thing.
XML can be generated in Yaws with the ehtml data type. The content type1 should be
set to application/xml and the top line should be set to a standard XML declaration
similar to this:
<?xml version="1.0" encoding="utf-8"?>
Alternatively, a template engine like ErlyDTL (see “ErlyDTL” on page 26) can be used
to make XML as in Example 2-20.
In addition to generating XML with the ehtml type, it is also possible to generate it with
the xmerl package included with Erlang, and parse it with xmerl_scan.
It will also often be necessary to scan an existing XML document. This can be done
with the xmerl_scan package that is included with Erlang. There are two basic functions
to do this, file/1 and string/1. The file/1 function will take the path to a file on disk
Building the Response | 49
as a parameter, while string/1 will take the XML in a string that is already in memory.
There are also versions of both that allow the programmer to specify a number of
options in a second parameter. Check the xmerl_scan man page for all the possible
options.
The data structure that is created when you run xmerl_scan:file/1 is rather long. For
the XML shown in Example 4-10, it will generate data as shown in Example 4-11. To
extract a specific element from this data structure it is possible to use XPATH via the
xmerl_xpath module.
Example 4-10. Sample XML
<?xml version="1.0" encoding="utf-8"?>
<user>
<id>31415926</id>
<name>Joe Armstrong</name>
<note>Created Erlang</note>
</user>
Example 4-11. Parsed XML
{{xmlElement,user,user,[],
{xmlNamespace,[],[]},
[],1,[],
[{xmlText,[{user,1}],1,[],"\n ",text},
{xmlElement,id,id,[],
{xmlNamespace,[],[]},
[{user,1}],
2,[],
[{xmlText,[{id,2},{user,1}],1,[],"31415926",text}],
[],".",undeclared},
{xmlText,[{user,1}],3,[],"\n ",text},
{xmlElement,name,name,[],
{xmlNamespace,[],[]},
[{user,1}],
4,[],
[{xmlText,[{name,4},{user,...}],1,[],[...],...}],
[],undefined,undeclared},
{xmlText,[{user,1}],5,[],"\n ",text},
{xmlElement,note,note,[],
{xmlNamespace,[],[]},
[{user,1}],
6,[],
[{xmlText,[{...}|...],1,...}],
[],undefined,undeclared},
{xmlText,[{user,1}],7,[],"\n",text}],
[],".",undeclared},
[]}
50 | Chapter 4: Implementing REST
Responding to the REST Request
When the user sends a POST request to the web server, that is the key to create a new
airport record. The handler needs to find the airport name and other information from
the POST content with yaws_api:postvar/2, and then should create a new airport with
airport:create_airport/5. Example 4-12 takes the airport name and other informa-
tion, creates an airport record, and inserts it into the Mnesia database. The nice thing
about Mnesia is that if it is set up correctly, data will automatically be replicated across
a cluster.
Normally, when responding to a HTTP request, we return a status of 200 OK. However,
here we are creating a new resource, so returning a status of 201 Created makes sense.
The body could be blank or contain any relevant information such as the name and ID
of the airport. In this case we return the JSON that was sent by the browser, as the
ExtJS framework expects that.
Example 4-12. Generating the content
handle('POST', Arg) ->
{ok, Json, _} = rfc4627:decode(Arg#arg.clidata),
io:format("~n~p:~p POST request ~p~n",
[?MODULE, ?LINE, Json]),
Airport
= rfc4627:get_field(Json, "airport", <<>>),
City
= rfc4627:get_field(Json, "city", <<>>),
Country
= rfc4627:get_field(Json, "country", <<>>),
Name
= rfc4627:get_field(Json, "name", <<>>),
_Status = addAirport(Airport, City, Country, Name),
[{status, 201},
{html, Arg#arg.clidata}];
A Full Example
So far this chapter has used little bits of code to show how to do different parts of a
service. This section will take those bits and unify them into a complete service that
can be used as a basis for your own applications. Most (but not all) of the code here is
from the previous sections.
In general, a REST service will want to do one of two things: either work records in
Mnesia or another data store, or interact with some form of backend application by
sending messages back and forth. Here is a full example using Mnesia.
In this example, when a GET event comes in, it will query Mnesia, return a list of all
the airports, and return them to the user.
When the user sends a POST request, the system will add a new record to the Mnesia
data store. If needed we could also take other actions here, such as invalidating a cache
or calling other functions to take other actions.
A Full Example | 51
When the user sends a PUT request, we will update an existing record. In this case we
will look it up by its IATA code and update the airport for new information. We cannot
handle the case where an airport changes its IATA code, but this should be rare enough
a case that we could delete the record and create it again.
When the user sends a DELETE request, we will delete the record from the data store.
There is also an extra clause at the end to catch any requests that are not one of the
four major HTTP requests and return a “405 Method Not Allowed” response.
In order for all this to work, we need to have an airport data format; in this case it is
very simple and shown in Example 4-2. This record includes only the airport IATA
code, name, city, and country.
We must also set up a table in the Mnesia data store, as in Example 4-13. This must be
done before the code is run and normally would be done in an .erlang file that Yaws
will run on startup.
The calls to io:format serialize all server activity through the IO server;
remove them for production.
Example 4-13. Setting up Mnesia
%% Add this to the .erlang file
application:start(mnesia).
mnesia:create_table(airport,
[
{attributes,record_info(fields, airport)},
{index, [country]}]).
Example 4-14 brings all of the airport example code together.
Example 4-14. Full airport example
-module(rest).
-include("/usr/lib/erlang/lib/stdlib-1.17.3/include/qlc.hrl").
-include("/usr/lib/yaws/include/yaws_api.hrl").
-export([out/1, addAirport/4, handle/2]).
%-compile(export_all).
-define(RECORD_TYPE,
airport).
-define(RECORD_KEY_FIELD, code).
-record(?RECORD_TYPE,
{?RECORD_KEY_FIELD, city, country, name }).
out(Arg) ->
Method = method(Arg) ,
io:format("~p:~p ~p Request ~n", [?MODULE, ?LINE, Method]),
handle(Method, Arg).
52 | Chapter 4: Implementing REST
method(Arg) ->
Rec = Arg#arg.req,
Rec#http_request.method.
convert_to_json(Lines) ->
Data = [{obj,
[{airport, Line#?RECORD_TYPE.code},
{city,
Line#?RECORD_TYPE.city},
{country, Line#?RECORD_TYPE.country},
{name,
Line#?RECORD_TYPE.name}]}
|| Line <- Lines],
JsonData = {obj, [{data, Data}]},
rfc4627:encode(JsonData).
addAirport(Code, City, Country, Name) ->
NewRec = #?RECORD_TYPE{
?RECORD_KEY_FIELD
= Code,
city
= City,
country
= Country,
name
= Name},
io:format("~p:~p Adding Airport ~p~n",
[?MODULE,?LINE, NewRec]),
Add = fun() ->
mnesia:write(NewRec)
end,
{atomic, _Rec} = mnesia:transaction(Add),
NewRec.
handle('GET', _Arg) ->
io:format("~n ~p:~p GET Request ~n", [?MODULE, ?LINE]),
Records = do(qlc:q([X || X <- mnesia:table(?RECORD_TYPE)])),
Json = convert_to_json( Records),
io:format("~n ~p:~p GET Request Response ~p ~n", [?MODULE, ?LINE, Json]),
{html, Json};
handle('POST', Arg) ->
{ok, Json, _} = rfc4627:decode(Arg#arg.clidata),
io:format("~n~p:~p POST request ~p~n",
[?MODULE, ?LINE, Json]),
Airport
= rfc4627:get_field(Json, "airport", <<>>),
City
= rfc4627:get_field(Json, "city", <<>>),
Country
= rfc4627:get_field(Json, "country", <<>>),
Name
= rfc4627:get_field(Json, "name", <<>>),
_Status = addAirport(Airport, City, Country, Name),
[{status, 201},
{html, Arg#arg.clidata}];
handle('PUT', Arg) ->
[IndexValue,_] = string:tokens(Arg#arg.pathinfo),
{ok, Json, _} = rfc4627:decode(Arg#arg.clidata),
io:format("~p:~p PUT request ~p ~p~n",
A Full Example | 53
[?MODULE, ?LINE, IndexValue, Json]),
Airport
= rfc4627:get_field(Json, "airport", <<>>),
City
= rfc4627:get_field(Json, "city", <<>>),
Country
= rfc4627:get_field(Json, "country", <<>>),
Name
= rfc4627:get_field(Json, "name", <<>>),
NewRec = #?RECORD_TYPE{
?RECORD_KEY_FIELD
= Airport,
city
= City,
country
= Country,
name
= Name},
io:format("~p:~p Renaming ~p",
[?MODULE, ?LINE, NewRec]),
ChangeName = fun() ->
mnesia:delete(
{?RECORD_KEY_FIELD, IndexValue}),
mnesia:write(NewRec)
end,
{atomic, _Rec} = mnesia:transaction(ChangeName),
[{status, 200},
{html, IndexValue}];
handle('DELETE', Arg) ->
[IndexValue, _ ] = string:tokens(Arg#arg.pathinfo),
io:format("~p:~p DELETE request ~p",
[?MODULE, ?LINE, IndexValue]),
Delete = fun() ->
end,
mnesia:delete(
{?RECORD_KEY_FIELD, IndexValue})
Resp = mnesia:transaction(Delete),
case Resp of
{atomic, ok} ->
[{status, 204}];
{_, Error} ->
io:format("~p:~p Error ~p ",
[?MODULE, ?LINE, Error]),
[{status, 400},
{html, Error}]
end;
handle(Method,_) ->
[{error, "Unknown method " ++ Method},
{status, 405},
{header, "Allow: GET, HEAD, POST, PUT, DELETE"}
].
do(Q)->
54 | Chapter 4: Implementing REST
F = fun() ->
qlc:e(Q)
end,
{atomic, Value} = mnesia:transaction(F),
Value.
Finally, we need a frontend to use all this with. I created a simple frontend in Coffee-
Script with ExtJS (see http://sencha.com) and it is included in Example 4-15. This creates
a UI in the browser that looks like Figure 4-1.
Example 4-15. CoffeeScript frontend (airport.coffee)
makeModel = ->
Ext.define("Airport",
extend: "Ext.data.Model",
fields:[
{name: "airport"}
{name: "city"}
{name: "country"}
{name: "name"}
]
)
makeStore = ->
model = makeModel()
store = Ext.create("Ext.data.Store",
autoLoad : true
autoSync : true
model
: model
proxy
:
type
: "rest"
url
: "airports.yaws" # Will need to change the backend here
reader :
type: "json"
root: "data"
writer:
type: "json"
)
setupAirports = ->
store
= makeStore()
rowEditing = Ext.create "Ext.grid.plugin.RowEditing"
grid
= Ext.create "Ext.grid.Panel"
renderTo : document.body
plugins : [rowEditing]
width
: 500
height
: 300
title
: "Airports"
store
: store
columns:
[
{
text
: 'Airport',
width
: 60
sortable : true
A Full Example | 55
}
{
}
{
}
{
]
dockedItems:
[
}
dataIndex : "airport"
editor
: {allowBlank: false}
text : "City"
dataIndex : "city"
sortable : true
editor : {allowBlank: false}
text : "Country"
dataIndex : "country"
sortable : true
editor : {allowBlank: false}
text : "Airport Name"
dataIndex : "name"
sortable : true
editor : {allowBlank: false}
xtype: "toolbar"
items:
[
{
}
{
]
Ext.onReady setupAirports
56 | Chapter 4: Implementing REST
]
}
text: "Add"
handler: ->
store.insert(0, new Airport())
rowEditing.startEdit(0,0)
itemId: 'delete'
text: "Delete"
handler: () ->
selection = grid
.getView()
.getSelectionModel()
.getSelection()[0]
if(selection)
store.remove(selection)
Figure 4-1. Airports UI in a browser
A Full Example | 57
