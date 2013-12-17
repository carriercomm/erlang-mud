.SUFFIXES: .erl .beam .yrl
.erl.beam:
	erlc -W +debug_info $<
.yrl.beam:
	erlc -W $<
ERL = erl -boot start_clean
MODS = comms control_service generic_ai hci location_service mud npc partial_ai trie util
all: compile
compile: ${MODS:%=%.beam} 

