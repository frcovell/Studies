#!/bin/bash
#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=1:mem=1gb
#PBS -J 1-960

## How many different burn ins am I going to do?
## 4 different dispersal distances (0, 1, 1.5, 2.5)
## 3 different interaction distances (0 , 1, 1.5)
## 4 different Intraspecific interactions 0, -0.05, -0.1, -0.5
## 4 different immigrations rates 0, 0.000016529, 0.00016529, 0.02

## So that is 4*3*4*4 = 192
## But I want to run each of these for 5 communties
## So 192*5 = 960

## So I need 960 different seeds
seeds=()
for s in $(seq 100 100 96000)
do
    seeds+=("$s")
done

# echo "${args[@]}"

DispDist=(0 1 1.5 2.5)
InteractDist=(0 1 1.5)
IntraComp=(0 -0.05 -0.1 -0.5)
Immigrat=(0 0.000016529 0.00016529 0.002)

args=()
s=0

    for d in "${DispDist[@]}";
    do
        for inter in "${InteractDist[@]}"
        do
            for intra in "${IntraComp[@]}"
            do
                for imm in "${Immigrat[@]}"
                do
                ## Doing this 5 times to create comparable communities
                ## probably need to streamline this bit going forward
                    args+=("${seeds[s]}" "$d" "$inter" "$intra" "$imm")
                    s=$((s+1))
                    args+=("${seeds[s]}" "$d" "$inter" "$intra" "$imm")
                    s=$((s+1))
                    args+=("${seeds[s]}" "$d" "$inter" "$intra" "$imm")
                    s=$((s+1))
                    args+=("${seeds[s]}" "$d" "$inter" "$intra" "$imm")
                    s=$((s+1))
                    args+=("${seeds[s]}" "$d" "$inter" "$intra" "$imm")
                    s=$((s+1))
                done
            done
        done
    done

cp $HOME/TNM_HPC_Template.cpp $TMPDIR
g++ TNM_HPC_Template.cpp

./a.out ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 5)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 4)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 3)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 2)]} ${args[$( expr "$PBS_ARRAY_INDEX" '*' 5 - 1)]}
