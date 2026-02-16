
// Implémentation en Promela de l'algorithme 
// d'élection de leader de Dolev-Klawe-Rodeh

#define N 3
#define ACTIVE 1
#define PASSIVE 0
#define LEADER 2

chan channel[N] = [1] of {int,int};

active[N] proctype p(){
    int leader = _pid;
    int received = 0;
    int sent = 0;
    int mode = ACTIVE;
    int ldf = 0;
    int nbIteration = 0;

    do
        :: mode == ACTIVE -> {
            nbIteration++;
            assert(nbIteration <= N+1);
            sent = leader;
            channel[(_pid+1)%N] ! sent, ldf;
            channel[_pid] ? received, ldf;
            if
                :: received == leader ->
                    mode = LEADER;
                    ldf = 1;
                    channel[(_pid+1)%N] ! sent, ldf;
                :: else -> {
                    sent = received;
                    channel[(_pid+1)%N] ! sent, ldf;
                    channel[_pid] ? received, ldf;
                    if
                        :: (sent <= leader || sent <= received) ->
                            mode = PASSIVE;
                        :: else ->
                            leader = sent;
                    fi
                }
            fi
        }

        :: mode == PASSIVE ->{
            if
                :: ldf == 1 -> break; 
                :: else -> {
                    channel[_pid] ? received, ldf;
                    sent = received;
                    channel[(_pid+1)%N] ! sent, ldf;
                }
            fi
        }

        :: ldf == 1 -> break;
    od
}


// ltl EventuallyOneLeader { <> ((p[0]:mode == 2) ) }

// ltl NoTwoLeaders { [] ((p[0]:mode == 2 -> p[1]:mode != 2 && p[2]:mode != 2) &&
//                    (p[1]:mode == 2 -> p[0]:mode != 2 && p[2]:mode != 2) &&
//                    (p[2]:mode == 2 -> p[0]:mode != 2 && p[1]:mode != 2)) }

// ltl LeaderIsMax { []((p[2]:mode == 2 -> (p[2]:leader >= p[0]:leader && p[2]:leader >= p[1]:leader))
//                     && (p[1]:mode == 2 -> (p[1]:leader >= p[0]:leader && p[1]:leader >= p[2]:leader))
//                     && (p[0]:mode == 2 -> (p[0]:leader >= p[1]:leader && p[0]:leader >= p[2]:leader))) }

// ltl nbIteration { [] ((p[0]:nbIteration <= N+1) &&
//                     (p[1]:nbIteration <= N+1) &&
//                     (p[2]:nbIteration <= N+1)) }