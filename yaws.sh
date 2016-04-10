case $(hostname) in
	dump.bitcheese.net) OPTS="--nodebug --daemon";;
	bitcheese.net) OPTS="--nodebug --daemon";;
	*) OPTS=""
esac
yaws --conf yaws.conf --mnesiadir Mnesia.dump@$(hostname) -name dump@$(hostname) $OPTS
