getContract <- function(primary_id) {
    instr <- getInstrument(primary_id)
    instr$IB
}

