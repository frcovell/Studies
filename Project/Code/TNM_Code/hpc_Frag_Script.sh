#!/bin/bash
#PBS -l walltime=48:00:00
#PBS -l select=1:ncpus=1:mem=1gb
#PBS -J 1-960

## How many runs do I need?
## So I have 960 burn in communities
## Each of these needs to run for the 11 fragment types, and each of them then needs to run 5 times
## So that is just 960*11*5 = 52,800
## And each original community (seed) will have 20 runs after it, so it will go 101, 102.... 120; 201, 202....220

# Array of parameters
DispDist=(0 1 1.5 2.5)
InteractDist=(0 1 1.5)
IntraComp=(0 -0.05 -0.1 -0.5)
Immigrat=(0 0.000016529 0.00016529 0.002)
fragmentNames=("Landscape1_1.txt" "Landscape1_0.txt" "AmalgamationLandscape1_0.1.txt" "AmalgamationLandscape1_0.2.txt" "AmalgamationLandscape1_0.3.txt" "AmalgamationLandscape1_0.4.txt" "AmalgamationLandscape1_0.5.txt" "AmalgamationLandscape1_0.6.txt" "AmalgamationLandscape1_0.7.txt" "AmalgamationLandscape1_0.8.txt" "AmalgamationLandscape1_0.9.txt")

# # array of every combination
args=()
s=100

for d in "${DispDist[@]}"; 
do
    for inter in "${InteractDist[@]}"; 
    do
        for intra in "${IntraComp[@]}";
        do
            for imm in "${Immigrat[@]}";
            do
                for i in $(seq 1 5)
                do
                    nS=$((s+1))
                    for frag in "${fragmentNames[@]}";
                    do
                        for j in $(seq 1 5)
                        do
                            args+=("$s" "$nS" "$d" "$inter" "$intra" "$imm" 1 "$frag")
                            nS=$((nS+1))
                        done
                    done 
                    s=$((s+100))
                done
            done
        done
    done
done   


# # Running TMN Via PBS_Array_Index
cp $HOME/TNM_HPC_Template.cpp $TMPDIR
g++ TNM_HPC_Template.cpp

# ./a.out ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 8)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 7)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 6)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 5)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 4)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 3)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 2)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 8 - 1)]}

# (nFragments * nSeeds) = nRuns
# 11 * 5
# pramaters * nRuns
# 8 * 55
for i in $(seq 0 8 440)
do
    ./a.out ${args["$PBS_ARRAY_INDEX" * 160 - (160 - $i)]} ${args["$PBS_ARRAY_INDEX" * 160 - (159 - $i)]} ${args["$PBS_ARRAY_INDEX" * 160 - (158 - $i)]} ${args["$PBS_ARRAY_INDEX" * 160 - (157 - $i)]} ${args["$PBS_ARRAY_INDEX" * 160 - (156 - $i)]} ${args["$PBS_ARRAY_INDEX" * 160 - (155 - $i)]} ${args["$PBS_ARRAY_INDEX" * 160 - (154 - $i)]} ${args["$PBS_ARRAY_INDEX" * 160 - (153 - $i)]}
done
