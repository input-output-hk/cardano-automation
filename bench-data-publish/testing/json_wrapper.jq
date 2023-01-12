# fully populate a JSON as payload for import
def combine(m):
    { meta:         m
    , blockprop:    $bprop
    , clusterperf:  $cperf
    };

# normalizes data from meta.json to match Cardano.Benchmarking.Publish.Types.MetaStub
def normalize($m):
    { profile:      $m.meta.profile
    , commit:       $m.meta.pins."cardano-node"
    , timestamp:    $m.meta.timestamp
    };

# wrap everything up into Main.JSONWrapper
def wrap(p):
    { command:      $cmd
    , schema:       $schema
    , payload:      p
    };
