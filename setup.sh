#!/bin/bash
if [[ $1 == 'rv32_5stage' ]]; then
	sbt 'runMain rv32_5stage.Top -td ../mizhi/emulator/rv32_5stage/generated-src'
elif [[ $1 == 'chiwen' ]]; then
	sbt 'runMain chiwen.Top -td ../mizhi/emulator/chiwen/generated-src'
elif [[ $1 == 'fuxi' ]]; then
	sbt 'runMain fuxi.Top -td ../mizhi/emulator/fuxi/generated-src'
elif [[ $1 == 'bian' ]]; then
	sbt 'runMain bian.Top -td ../mizhi/emulator/bian/generated-src'	
elif [[ $1 == 'rv32_3stage' ]]; then
	sbt 'runMain rv32_3stage.Top -td ../mizhi/emulator/rv32_3stage/generated-src'
fi
