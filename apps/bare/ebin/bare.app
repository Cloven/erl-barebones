{application,bare,
             [{description,[]},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib,folsom,eredis,cowboy,lager]},
              {mod,{bare_app,[]}},
              {env,[{client_timeout,60000}]},
              {modules,[bare_app,bare_engine,bare_sup,ws_handler]}]}.
