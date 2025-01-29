-- switches the global conflict strategy to automerge, which avoids the need to retry statements during heavy concurrent edge creation
ALTER database CONFLICTSTRATEGY "automerge";