#!/bin/bash


BINARY=python3.9
case "$(uname -s)" in
   Linux*) BINARY=python3;;
esac

SCRIPT=compare.py

WINDOWS=(5 10 25)
for window in ${WINDOWS[*]}; do
   $BINARY $SCRIPT --file other_data/df_for_comparison_with_calimera_$window.csv \
                   --window $window \
                   --ref other_data/comp_table.tsv \
                   --output other_data \
                   --time 5 \
                   --name 'Jnk1_Dusp8_KOonHFD_TSE_clean' \
                   --metadata 'Jnk1_Dusp8_KOonHFD_TSE_clean_metadata'
exit
#

$BINARY $SCRIPT --file new_data4/df_for_comparison_with_calimera_$window.csv \
                   --window $window \
                   --ref new_data4/comp_table6.tsv \
                   --output new_data4 \
                   --time 15 \
                   --name '20220523_NonIMPC_Calimera_Males2_PR_2' \
                   --metadata '20220523_NonIMPC_Calimera_Males2_PR_2_metadata'


   $BINARY $SCRIPT --file new_data3/df_for_comparison_with_calimera_$window.csv \
                   --window $window \
                   --ref new_data3/comp_table5.tsv \
                   --output new_data3 \
                   --time 15 \
                   --name '20220405_For Calimera_Chow_HFD_F_PR_2' \
                   --metadata '20220405_For Calimera_Chow_HFD_F_PR_2_metadata'


   $BINARY $SCRIPT --file new_data2/df_for_comparison_with_calimera_$window.csv \
                   --window $window \
                   --ref new_data2/comp_table4.tsv \
                   --output new_data2 \
                   --time 15 \
                   --name '20220512_NonIMPC_Calimera_Males_DP_2' \
                   --metadata '20220512_NonIMPC_Calimera_Males_DP_2_metadata'


   $BINARY $SCRIPT --file new_data/df_for_comparison_with_calimera_$window.csv \
                   --window $window \
                   --ref new_data/comp_table3.tsv \
                   --output new_data \
                   --time 15 \
                   --name '20220515_NonIMPC_Calimera_Females2_DP_2' \
                   --metadata '20220515_NonIMPC_Calimera_Females2_DP_2_metadata'



   $BINARY $SCRIPT --file old_data/df_for_comparison_with_calimera_$window.csv \
                   --window $window \
                   --ref old_data/comp_table.tsv \
                   --output old_data \
                   --time 10 \
                   --name 'Bl6j_vs_Bl6n_TSE' \
                   --metadata 'Bl6j_vs_Bl6n_TSE_metadata'
done
