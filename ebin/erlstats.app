{application, erlstats,
 [{description, "Erlang irc services framework"},
  {vsn, "1"},
  {modules, [erlstats_app, erlstats,
	     erlstats_supervisor, esmisc, genmnesiatables,
	     ts6, greasel, fricka, stats, hex,
	     erlstats_plugins_supervisor]},
  {registered, [erlstats]},
  {applications, [kernel, stdlib, sasl, crypto]},
  {mod, {erlstats_app,[]}}
 ]}.
