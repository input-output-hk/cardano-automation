# bench-data-publish


## Purpose 

The command-line tool `bench-data-publish` is a utility for publishing the result of benchmarking run analysis to a Postgres DB.

It relies on separate SQL definitions of 1) a DB schema (i.e. tables for data) and 2) DB views on the data stored within.

1. The schema is designed to only utilize an absolute minimum of data normalization; the `.json`s from a run and its analysis are stored verbatim the schema tables.
2. The views extract all relevant properties and metrics from those objects and expose them to make the data searchable and filterable.

By using a service like PostgREST, a REST API can be served immediately from these views, without an addtional layer of configuration.


## Usage 

```
Usage: bench-data-publish (COMMAND | COMMAND 
                            [--pg-uri URI | --db ARG [-u ARG] [-p ARG] [-h ARG] 
                              [-P ARG]] [-s SCHEMA] [-f])

Available options:
  --pg-uri URI             postgres[ql]:// URI of DB (default: $BENCH_DATA_PGURI)
  --db ARG                 DB name
  -u ARG                   DB user name (default: <your login name>)
  -p ARG                   DB password (default: $BENCH_DATA_PASS)
  -h ARG                   DB host (default: localhost)
  -P ARG                   DB port (default: 5432)
  -s SCHEMA                DB schema to use (default: public)
  -f                       Force destructive operations (e.g. bootstrap)

Available commands:
  via-stdin                Expect command and payload as JSON via stdin
  import                   Import/update specified run
  import-all               Import/update all runs contained in directory
  list                     List all runs
  publish                  Publish specified run to API
  unpublish                Unpublish specified run from API
  bootstrap                Bootstrap schema onto DB, CLEARING PREVIOUS SCHEMA
  update-views             Only update API facing views for role in the schema, not touching any tables or stored data
```

### DB resource

1. The specification of a DB resource is mandatory, either as a Postgres URI or via individual values.
   * The DB user should be owner of the DB specified; it needs privileges to `CREATE`, `DROP` and `GRANT`.
2. The specification of a DB schema is optional, defaulting to `public`.
   * All DB objects that `bench-data-publish` touches reside in that schema only.
3. For the `update-views` commmand, an unprivileged / anonymous DB role is expected.
   * This role is intended for any API-side read-only queries; for this role, `CREATE ROLE <anon_role> NOLOGIN;` is sufficient.

### API via PostgREST

The API which PostgREST will eventually deliver is defined implicitly by the views in `db/bench-data-views.sql`.

When `bench-data-publish` makes relevant changes to data or views, it runs appropriate actions for rebuilding materialized view(s), and notifying PostgREST to rebuild its schema cache. PostgREST itself will not need a restart.

## Wrapping the CLI call in JSON

Should the binary run on a remote machine, or in an isolated manner (e.g. inside a container), it is possible to pass the actual command, options and possibly data wrapped in JSON. The command `via-stdin` tells the binary to expect such an object piped through `stdin`.

When `via-stdin` is used, no other command line options are expected. The result is represented as another JSON which can be received from `stdout`.

The serializations for those objects are defined as `JSONWrapper.JSONWrapper` and `JSONWrapper.JSONResult`. `Main.toJSONWrapper` defines how command line arguments are encoded as JSON, which has to exactly match the `payload` attribute in the serialization. In `testing/` you can find a shell script (using `jq`) that conveniently constructs JSON values to pass via `stdin`.
