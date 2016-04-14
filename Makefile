PROJECT = barebones
DEPS = lager
dep_lager_commit = 2.1.0

EXTRA_ERLC_OPTS = +'{parse_transform, lager_transform}'
include erlang.mk
