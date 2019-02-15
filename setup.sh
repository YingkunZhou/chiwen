#!/bin/bash
if [[ $1 == 'rv32_5stage' ]]; then
	sbt 'runMain rv32_5stage.Top -td /home/albert/CPU/mizhi/emulator/rv32_5stage/generated-src'
elif [[ $1 == 'chiwen' ]]; then
	sbt 'runMain chiwen.Top -td /home/albert/CPU/mizhi/emulator/chiwen/generated-src'
elif [[ $1 == 'fuxi' ]]; then
	sbt 'runMain fuxi.Top -td /home/albert/CPU/mizhi/emulator/fuxi/generated-src'
elif [[ $1 == 'rv32_3stage' ]]; then
	sbt 'runMain rv32_3stage.Top -td emulator/rv32_3stage/generated-src'
fi
