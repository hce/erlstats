{application, erlstats,
 [{description, "Erlang irc services framework"},
  {vsn, "1"},
  {modules, [erlstats_app, erlstats,
	     erlstats_supervisor, esmisc,
	     ts6]},
  {registered, [erlstats]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {erlstats_app,[]}}
 ]}.
