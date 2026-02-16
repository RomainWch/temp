#!/bin/sh

# modify algo according to your needs (use -v if needed)
BMCFLAGS='-algo global'

# Timeout set to 10s
ulimit -t 10

# Bounds to test
BOUNDS="5 25 100"

# Forward analysis
echo "####################"
echo "# Forward Analysis #"
echo "####################"
echo

for case in aut/*.aut prg/*.c; do
	for k in $BOUNDS; do
		echo "============================="
		echo "$case with bound $k"
		echo "============================="
		../bmc.d.byte $BMCFLAGS -analysis fwd -bound $k $case
		echo
	done
done

# Backward analysis
echo "####################"
echo "# Backward Analysis #"
echo "####################"
echo

for case in aut/*.aut prg/*.c; do
	for k in $BOUNDS; do
		echo "============================="
		echo "$case with bound $k"
		echo "============================="
		../bmc.d.byte $BMCFLAGS -analysis bwd -bound $k $case
		echo
	done
done
