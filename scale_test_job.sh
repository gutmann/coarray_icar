#!/bin/bash
#PBS -A P48500028
#PBS -N SCALE_TEST
#PBS -j oe
#PBS -q regular
#PBS -l walltime=12:00:00
#PBS -l select=50:ncpus=36:mpiprocs=36

function run_test(){
    EXE=./test-ideal

    # for (( i=1440; i>360; i-=18 )); do
    for (( i=64; i>1; i-=2 )); do
        echo -n "[" $i ","
        cafrun -np $i ${EXE} | grep "Model run time" | sed  -e's/Model run time:/ /;s/$/],/;s/seconds//'
    done
    echo -n "[ 1, "
    cafrun -np 1 ${EXE} | grep "Model run time" | sed  -e's/Model run time:/ /;s/$/]/;s/seconds//'
}

cat <<EOF
#!/usr/bin/env python
# `cat input-parameters.txt`

import numpy as np
import matplotlib.pyplot as plt

d=np.array([
EOF

run_test

cat <<EOF
])
plt.plot(d[:,0], d[:,0]  / d[-1,0],"o",color="C1", label="Ideal")
plt.plot(d[:,0], d[-1,1] / d[:,1], "x",color="C0", label="CAF")

plt.xlabel("Number of Processors")
plt.ylabel("Speedup")
plt.legend( )
plt.tight_layout( )
plt.title("`cat input-parameters.txt`")
plt.savefig("speedup.png")

plt.clf( )
plt.plot(d[:,0], d[:,1], "x",color="C0", label="CAF")
plt.xlabel("Number of Processors")
plt.ylabel("Run time (s)")
plt.legend( )
plt.title("`cat input-parameters.txt`")
plt.savefig("runtime.png")
EOF

# Example output
# Creates a python script that will plot the scaling data

#!/usr/bin/env python
# import numpy as np
# import matplotlib.pyplot as plt
#
# d=np.array([
# [ 180 ,     5.9            ],
# [ 144 ,     7.3            ],
# [ 108 ,     9.4            ],
# [ 72 ,    13.6            ],
# [ 36,    26.3            ]
# ])
# plt.plot(d[:,0],d[:,0]/d[-1,0],"o",color="C1", label="Ideal")
# plt.plot(d[:,0],d[-1,1]/d[:,1],"x",color="C0", label="CAF")
#
# plt.xlabel("Number of Processors")
# plt.ylabel("Speedup")
# plt.tight_layout( )
# plt.savefig("speedup.png")
#
# plt.clf( )
# plt.plot(d[:,0], d[:,1], "x",color="C0", label="CAF")
# plt.xlabel("Number of Processors")
# plt.ylabel("Run time (s)")
# plt.legend( )
# plt.tight_layout( )
# plt.savefig("runtime.png")
