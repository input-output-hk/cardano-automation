# bench-data-publish


## Purpose 

The command-line tool `bench-data-publish` is a utility for publishing the result of benchmarking run analysis to a Postgres DB.

It relies on separate SQL definitions of 1) a DB schema (i.e. tables for data) and 2) DB views on the data stored within.

1. The schema is designed to only utilize an absolute minimum of data normalization; the `.json`s from a run and its analysis are stored verbatim the schema tables.
2. The views extract all relevant properties and metrics from those objects and expose them to make the data searchable and filterable.

By using a service like PostgREST, a REST API can be served immediately from these views, without an addtional layer of configuration.


## Usage 

```
Usage: bench-data-publish COMMAND
                          [--pg-uri URI | --db ARG [-u ARG] [-p ARG] [-h ARG]
                            [-P ARG]] (-s|--schema SCHEMA) [-f]

Available options:
  --pg-uri URI             postgres[ql]:// URI of DB (default: $BECNH_DATA_PGURI)
  --db ARG                 DB name
  -u ARG                   DB user name (default: <your login name>)
  -p ARG                   DB password (default: $BENCH_DATA_PASS)
  -h ARG                   DB host (default: localhost)
  -P ARG                   DB port (default: 5432)
  -s,--schema SCHEMA       DB schema to use
  -f                       Force destructive operations (e.g. bootstrap)

Available commands:
  import                   Import/update specified run
  import-all               Import/update all runs contained in directory
  list                     List all runs
  publish                  Publish specified run to API
  unpublish                Unpublish specified run from API
  bootstrap                Bootstrap schema onto DB, CLEARING PREVIOUS SCHEMA
  update-views             Update API facing views in the schema only, not
                           touching any tables or stored data
```

### DB resource

1. The specification of a DB resource is mandatory, either as a Postgres URI or via individual values.
   * The DB user should be owner of the DB specified; it needs privileges to `CREATE`, `DROP` and `GRANT`.
2. The specification of a DB schema is mandatory; all DB objects that `bench-data-schema` touches reside in that schema only.
3. For the `bootstrap` and `update-views` commmands, an unprivileged / anonymous DB role is needed.
   * This role is intended for any API-side queries; for this role, `CREATE ROLE <anon_role> NOLOGIN;` is sufficient.

### API via PostgREST

The API which PostgREST will eventually deliver is defined implicitly by the views in `db/bench-data-views.sql`.

When `bench-data-publish` makes relevant changes to data or views, it runs appropriate actions for rebuilding materialized view(s), and notifying PostgREST to rebuild its schema cache. PostgREST itself will not need a restart.
