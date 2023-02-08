SHELL = /bin/bash
R    ?= R 
RCMD  =@$(R) CMD
TIMESTAMP = $(shell  date '+%Y-%m-%d-%H-%M')
here=${PWD}/..
all: full_mars_500_0.95_11571.RData full_cart_100_0.95_87320.RData full_mars_100_0.9_12204.RData full_cart_100_0.9_92199.RData full_cart_5000_0.95_81580.RData full_lm_5000_0.9_90597.RData full_mars_1000_0.95_96812.RData full_cart_500_0.9_19242.RData full_lm_500_0.95_11571.RData full_mars_1000_0.95_72611.RData full_mars_1000_0.9_6519.RData full_mars_500_0.9_99526.RData full_cart_5000_0.95_11571.RData full_lm_500_0.95_87320.RData full_cart_500_0.9_36244.RData full_cart_1000_0.95_32618.RData full_cart_100_0.95_19242.RData full_cart_500_0.95_69586.RData full_lm_1000_0.9_12204.RData full_lm_500_0.9_19242.RData full_mars_500_0.95_87320.RData full_mars_5000_0.9_25173.RData full_lm_1000_0.95_43809.RData full_cart_500_0.95_87320.RData full_lm_500_0.95_19242.RData full_mars_1000_0.95_43809.RData full_mars_1000_0.95_92490.RData full_mars_1000_0.95_6519.RData full_cart_500_0.9_13903.RData full_lm_500_0.9_69586.RData full_cart_1000_0.9_11571.RData full_lm_1000_0.95_6519.RData full_cart_500_0.95_19242.RData full_lm_1000_0.95_25173.RData full_cart_500_0.95_81580.RData full_mars_100_0.95_72611.RData full_lm_1000_0.95_36244.RData full_cart_100_0.95_92490.RData full_mars_500_0.9_11571.RData full_mars_500_0.9_43307.RData full_lm_1000_0.95_87320.RData full_mars_500_0.95_43307.RData full_lm_100_0.9_59521.RData full_lm_500_0.95_59521.RData full_lm_500_0.95_90597.RData full_cart_100_0.95_81580.RData full_cart_100_0.9_11571.RData full_mars_5000_0.95_32618.RData full_lm_100_0.95_25305.RData full_lm_1000_0.95_81580.RData full_lm_5000_0.9_8229.RData full_mars_1000_0.9_11571.RData full_cart_1000_0.95_43307.RData full_mars_1000_0.95_25305.RData full_lm_500_0.95_12204.RData full_lm_5000_0.9_22306.RData full_cart_100_0.95_96812.RData full_lm_5000_0.9_43307.RData full_mars_5000_0.9_90597.RData full_lm_100_0.95_72611.RData full_mars_100_0.95_6519.RData full_lm_100_0.95_92199.RData full_lm_100_0.9_8229.RData full_cart_500_0.95_36244.RData full_cart_500_0.9_43307.RData full_mars_500_0.95_25173.RData full_lm_5000_0.95_12204.RData full_mars_100_0.9_25305.RData full_mars_5000_0.9_87320.RData full_mars_1000_0.9_72611.RData full_lm_5000_0.9_24388.RData full_cart_500_0.95_22306.RData full_cart_1000_0.9_6519.RData full_lm_5000_0.9_32618.RData full_cart_500_0.9_22306.RData full_cart_500_0.9_96812.RData full_cart_1000_0.95_12204.RData full_lm_100_0.9_81580.RData full_cart_5000_0.95_8229.RData full_cart_100_0.9_8229.RData full_lm_1000_0.9_92490.RData full_mars_100_0.9_81580.RData full_mars_100_0.9_8229.RData full_mars_100_0.9_13903.RData full_cart_100_0.9_69586.RData full_cart_100_0.95_6519.RData full_cart_500_0.95_45399.RData full_mars_1000_0.95_99526.RData full_mars_1000_0.95_36244.RData full_lm_100_0.95_96812.RData full_cart_5000_0.9_36244.RData full_lm_100_0.95_19242.RData full_mars_100_0.9_92490.RData full_mars_1000_0.95_19242.RData full_mars_100_0.9_90597.RData full_mars_1000_0.95_12204.RData full_cart_500_0.95_6519.RData full_cart_500_0.95_11571.RData full_cart_100_0.9_59521.RData full_mars_1000_0.95_43307.RData full_cart_500_0.9_69586.RData full_mars_500_0.9_69586.RData full_lm_1000_0.9_90597.RData full_mars_500_0.95_19242.RData full_mars_500_0.95_13903.RData full_mars_500_0.95_96812.RData full_mars_1000_0.9_25173.RData full_lm_5000_0.9_25305.RData full_mars_500_0.9_87320.RData full_lm_100_0.9_43809.RData full_cart_500_0.95_12204.RData full_cart_500_0.95_25305.RData full_cart_100_0.95_69586.RData full_mars_1000_0.9_99526.RData full_lm_5000_0.9_59521.RData full_mars_100_0.9_6519.RData full_lm_5000_0.9_92490.RData full_lm_500_0.95_13903.RData full_cart_100_0.95_43307.RData full_lm_500_0.9_22306.RData full_lm_1000_0.95_92199.RData full_mars_100_0.9_45399.RData full_mars_1000_0.9_8229.RData full_cart_1000_0.9_59521.RData full_mars_5000_0.9_81580.RData full_lm_500_0.9_32618.RData full_mars_1000_0.95_69586.RData full_lm_5000_0.9_43809.RData full_mars_100_0.95_69586.RData full_lm_500_0.95_8229.RData full_cart_500_0.9_72611.RData full_mars_5000_0.9_99526.RData full_cart_500_0.9_12204.RData full_cart_100_0.95_99526.RData full_lm_500_0.95_43307.RData full_lm_1000_0.95_43307.RData full_lm_1000_0.9_19242.RData full_lm_500_0.95_96812.RData full_lm_5000_0.95_43307.RData full_cart_1000_0.9_92199.RData full_lm_5000_0.95_25173.RData full_cart_1000_0.95_13903.RData full_mars_100_0.9_87320.RData full_cart_5000_0.9_81580.RData full_mars_500_0.9_24388.RData full_mars_500_0.95_92199.RData full_mars_500_0.95_6519.RData full_mars_500_0.9_45399.RData full_mars_500_0.95_22306.RData full_cart_500_0.9_45399.RData full_lm_100_0.95_32618.RData full_cart_500_0.9_90597.RData full_cart_1000_0.9_92490.RData full_cart_100_0.95_12204.RData full_mars_1000_0.95_45399.RData full_cart_100_0.9_6519.RData full_mars_1000_0.9_92199.RData full_lm_1000_0.95_8229.RData full_cart_1000_0.9_43809.RData full_lm_5000_0.95_6519.RData full_cart_500_0.95_25173.RData full_cart_100_0.95_36244.RData full_lm_5000_0.95_24388.RData full_mars_5000_0.95_22306.RData full_mars_500_0.9_96812.RData full_mars_5000_0.95_99526.RData full_cart_5000_0.95_36244.RData full_mars_500_0.95_90597.RData full_cart_5000_0.9_13903.RData full_cart_100_0.95_24388.RData full_cart_100_0.95_43809.RData full_lm_1000_0.9_22306.RData full_lm_100_0.95_24388.RData full_cart_100_0.95_13903.RData full_mars_5000_0.9_6519.RData full_cart_500_0.95_92490.RData full_mars_5000_0.9_59521.RData full_mars_5000_0.9_92490.RData full_lm_1000_0.95_92490.RData full_cart_1000_0.9_36244.RData full_lm_1000_0.95_32618.RData full_lm_500_0.95_32618.RData full_mars_100_0.95_24388.RData full_lm_1000_0.9_81580.RData full_cart_1000_0.95_81580.RData full_mars_1000_0.9_69586.RData full_lm_1000_0.95_99526.RData full_lm_100_0.9_12204.RData full_cart_1000_0.95_92490.RData full_mars_5000_0.9_72611.RData full_lm_100_0.95_69586.RData full_cart_100_0.95_25305.RData full_mars_1000_0.9_19242.RData full_mars_1000_0.9_87320.RData full_mars_500_0.95_92490.RData full_lm_100_0.9_72611.RData full_lm_1000_0.95_22306.RData full_mars_5000_0.9_11571.RData full_cart_500_0.9_25305.RData full_mars_5000_0.9_96812.RData full_lm_1000_0.9_87320.RData full_lm_100_0.95_59521.RData full_lm_100_0.9_32618.RData full_lm_500_0.95_45399.RData full_cart_1000_0.95_92199.RData full_lm_100_0.9_96812.RData full_mars_1000_0.9_43307.RData full_cart_1000_0.9_13903.RData full_cart_1000_0.9_24388.RData full_mars_100_0.95_13903.RData full_cart_5000_0.95_96812.RData full_lm_1000_0.9_24388.RData full_lm_100_0.9_11571.RData full_lm_5000_0.9_19242.RData full_lm_5000_0.9_25173.RData full_mars_1000_0.9_45399.RData full_cart_1000_0.9_99526.RData full_cart_100_0.9_12204.RData full_mars_1000_0.9_13903.RData full_cart_500_0.95_24388.RData full_lm_5000_0.9_72611.RData full_mars_5000_0.9_12204.RData full_mars_100_0.9_11571.RData full_cart_5000_0.9_24388.RData full_cart_5000_0.95_92199.RData full_lm_100_0.9_87320.RData full_lm_1000_0.95_96812.RData full_lm_100_0.95_36244.RData full_mars_5000_0.9_19242.RData full_cart_5000_0.9_92490.RData full_lm_5000_0.9_13903.RData full_cart_1000_0.95_25305.RData full_lm_500_0.9_96812.RData full_lm_100_0.9_25305.RData full_mars_500_0.95_45399.RData full_mars_100_0.95_32618.RData full_mars_5000_0.9_92199.RData full_mars_1000_0.95_81580.RData full_lm_1000_0.9_13903.RData full_cart_5000_0.95_22306.RData full_cart_5000_0.9_43809.RData full_lm_100_0.9_90597.RData full_cart_5000_0.9_45399.RData full_lm_500_0.95_22306.RData full_cart_5000_0.9_72611.RData full_mars_500_0.95_8229.RData full_cart_5000_0.95_6519.RData full_cart_5000_0.9_43307.RData full_lm_1000_0.95_69586.RData full_mars_5000_0.95_25305.RData full_mars_500_0.95_59521.RData full_mars_500_0.9_32618.RData full_mars_500_0.95_72611.RData full_mars_5000_0.9_24388.RData full_lm_500_0.95_92199.RData full_lm_5000_0.9_36244.RData full_lm_5000_0.9_81580.RData full_lm_5000_0.95_72611.RData full_cart_100_0.9_99526.RData full_cart_1000_0.95_24388.RData full_mars_5000_0.95_11571.RData full_lm_100_0.9_92199.RData full_lm_500_0.95_6519.RData full_lm_100_0.9_13903.RData full_mars_5000_0.95_81580.RData full_cart_100_0.9_13903.RData full_cart_5000_0.95_19242.RData full_cart_500_0.95_92199.RData full_lm_5000_0.9_87320.RData full_mars_500_0.9_25305.RData full_lm_1000_0.9_36244.RData full_lm_100_0.9_22306.RData full_mars_1000_0.9_24388.RData full_lm_500_0.9_90597.RData full_cart_5000_0.95_24388.RData full_mars_1000_0.95_59521.RData full_cart_500_0.9_24388.RData full_mars_100_0.95_59521.RData full_lm_1000_0.95_19242.RData full_lm_5000_0.9_12204.RData full_cart_5000_0.9_59521.RData full_cart_1000_0.95_96812.RData full_lm_5000_0.9_45399.RData full_mars_100_0.9_99526.RData full_cart_500_0.95_99526.RData full_mars_100_0.95_96812.RData full_cart_5000_0.9_8229.RData full_mars_100_0.9_24388.RData full_mars_5000_0.95_92490.RData full_mars_100_0.95_19242.RData full_cart_100_0.9_81580.RData full_cart_100_0.95_8229.RData full_mars_100_0.9_96812.RData full_cart_1000_0.95_69586.RData full_lm_500_0.9_87320.RData full_cart_100_0.95_25173.RData full_mars_500_0.95_36244.RData full_cart_500_0.9_8229.RData full_lm_5000_0.95_36244.RData full_mars_5000_0.95_24388.RData full_lm_500_0.95_99526.RData full_cart_1000_0.95_59521.RData full_cart_100_0.9_90597.RData full_cart_1000_0.95_19242.RData full_cart_5000_0.95_72611.RData full_cart_1000_0.9_90597.RData full_lm_5000_0.9_69586.RData full_cart_1000_0.9_25305.RData full_lm_5000_0.95_13903.RData full_cart_500_0.9_59521.RData full_cart_100_0.95_32618.RData full_lm_1000_0.95_12204.RData full_lm_5000_0.9_92199.RData full_mars_5000_0.95_69586.RData full_lm_5000_0.95_92199.RData full_mars_100_0.95_11571.RData full_mars_100_0.9_25173.RData full_mars_1000_0.95_8229.RData full_mars_100_0.95_90597.RData full_lm_500_0.9_6519.RData full_lm_100_0.95_92490.RData full_lm_100_0.95_45399.RData full_cart_500_0.9_92199.RData full_mars_1000_0.9_96812.RData full_cart_100_0.9_22306.RData full_cart_100_0.9_87320.RData full_cart_5000_0.95_25305.RData full_lm_500_0.95_81580.RData full_mars_1000_0.95_22306.RData full_lm_5000_0.95_25305.RData full_cart_1000_0.9_8229.RData full_mars_500_0.9_72611.RData full_lm_500_0.9_12204.RData full_lm_5000_0.9_11571.RData full_mars_500_0.95_25305.RData full_cart_100_0.95_72611.RData full_cart_500_0.9_92490.RData full_mars_500_0.95_69586.RData full_lm_500_0.9_99526.RData full_mars_5000_0.95_90597.RData full_lm_500_0.9_92199.RData full_cart_5000_0.95_12204.RData full_mars_500_0.95_99526.RData full_cart_1000_0.95_72611.RData full_cart_500_0.9_43809.RData full_lm_1000_0.9_32618.RData full_lm_500_0.9_11571.RData full_cart_100_0.95_90597.RData full_lm_100_0.95_22306.RData full_cart_500_0.95_59521.RData full_cart_100_0.9_25305.RData full_lm_100_0.9_6519.RData full_cart_5000_0.95_59521.RData full_mars_500_0.9_22306.RData full_mars_5000_0.95_36244.RData full_cart_500_0.9_6519.RData full_lm_5000_0.95_11571.RData full_mars_500_0.9_92199.RData full_lm_100_0.95_8229.RData full_mars_500_0.9_19242.RData full_cart_5000_0.95_87320.RData full_mars_500_0.9_92490.RData full_mars_100_0.95_25305.RData full_mars_1000_0.9_32618.RData full_mars_100_0.95_43307.RData full_cart_5000_0.9_32618.RData full_lm_500_0.9_13903.RData full_lm_100_0.95_11571.RData full_cart_100_0.95_22306.RData full_cart_100_0.95_11571.RData full_cart_500_0.9_32618.RData full_mars_5000_0.95_45399.RData full_cart_100_0.9_25173.RData full_lm_5000_0.95_59521.RData full_mars_100_0.95_92199.RData full_mars_5000_0.9_22306.RData full_cart_500_0.95_43809.RData full_cart_500_0.95_43307.RData full_lm_500_0.9_45399.RData full_mars_5000_0.95_96812.RData full_cart_500_0.95_32618.RData full_lm_100_0.95_12204.RData full_lm_5000_0.95_19242.RData full_cart_5000_0.9_92199.RData full_cart_100_0.9_36244.RData full_cart_5000_0.9_25305.RData full_mars_1000_0.95_11571.RData full_cart_100_0.9_24388.RData full_mars_5000_0.95_12204.RData full_mars_1000_0.9_22306.RData full_mars_5000_0.95_6519.RData full_cart_5000_0.95_25173.RData full_mars_1000_0.95_92199.RData full_cart_5000_0.9_99526.RData full_cart_1000_0.95_99526.RData full_mars_1000_0.95_13903.RData full_lm_1000_0.95_11571.RData full_cart_5000_0.9_69586.RData full_lm_100_0.95_87320.RData full_lm_100_0.95_90597.RData full_lm_1000_0.9_25173.RData full_mars_500_0.95_32618.RData full_mars_100_0.9_36244.RData full_lm_500_0.9_8229.RData full_cart_100_0.9_19242.RData full_cart_100_0.95_92199.RData full_cart_500_0.95_13903.RData full_cart_5000_0.9_19242.RData full_lm_100_0.95_43307.RData full_mars_100_0.95_45399.RData full_mars_100_0.95_87320.RData full_cart_500_0.9_99526.RData full_lm_500_0.95_43809.RData full_mars_500_0.9_81580.RData full_mars_1000_0.9_36244.RData full_lm_1000_0.9_69586.RData full_lm_1000_0.9_43307.RData full_cart_1000_0.95_36244.RData full_mars_1000_0.9_92490.RData full_cart_1000_0.9_22306.RData full_lm_500_0.95_24388.RData full_mars_100_0.9_92199.RData full_lm_500_0.95_36244.RData full_cart_1000_0.95_6519.RData full_cart_1000_0.95_43809.RData full_mars_100_0.95_43809.RData full_lm_500_0.95_92490.RData full_lm_100_0.95_6519.RData full_cart_5000_0.95_13903.RData full_mars_100_0.95_81580.RData full_mars_5000_0.95_13903.RData full_cart_5000_0.95_90597.RData full_lm_500_0.9_24388.RData full_lm_1000_0.9_8229.RData full_cart_1000_0.9_12204.RData full_mars_5000_0.9_32618.RData full_mars_5000_0.95_19242.RData full_lm_1000_0.9_96812.RData full_lm_1000_0.95_24388.RData full_lm_500_0.9_25173.RData full_lm_1000_0.95_13903.RData full_lm_100_0.9_36244.RData full_cart_5000_0.95_43307.RData full_cart_100_0.9_43307.RData full_lm_5000_0.9_99526.RData full_mars_5000_0.9_45399.RData full_lm_100_0.9_24388.RData full_cart_1000_0.9_25173.RData full_mars_100_0.95_8229.RData full_cart_1000_0.9_19242.RData full_mars_500_0.9_43809.RData full_cart_5000_0.9_25173.RData full_cart_5000_0.9_11571.RData full_lm_500_0.9_81580.RData full_lm_500_0.95_69586.RData full_cart_100_0.9_32618.RData full_lm_500_0.9_72611.RData full_mars_500_0.9_25173.RData full_mars_500_0.9_8229.RData full_lm_5000_0.95_8229.RData full_lm_100_0.9_45399.RData full_mars_5000_0.95_43809.RData full_lm_500_0.95_72611.RData full_lm_1000_0.95_72611.RData full_cart_100_0.95_59521.RData full_mars_1000_0.95_24388.RData full_mars_500_0.9_6519.RData full_cart_1000_0.9_32618.RData full_lm_1000_0.95_25305.RData full_cart_500_0.95_96812.RData full_lm_5000_0.95_22306.RData full_mars_5000_0.95_72611.RData full_cart_500_0.95_8229.RData full_mars_100_0.9_43307.RData full_cart_1000_0.9_43307.RData full_cart_5000_0.95_99526.RData full_lm_100_0.9_43307.RData full_mars_1000_0.95_90597.RData full_lm_5000_0.95_87320.RData full_lm_1000_0.95_59521.RData full_lm_100_0.9_25173.RData full_lm_5000_0.95_99526.RData full_lm_500_0.95_25173.RData full_mars_1000_0.9_90597.RData full_mars_100_0.9_69586.RData full_cart_1000_0.9_69586.RData full_cart_500_0.9_11571.RData full_cart_500_0.95_72611.RData full_lm_100_0.95_43809.RData full_lm_5000_0.9_96812.RData full_lm_500_0.95_25305.RData full_mars_100_0.9_72611.RData full_cart_1000_0.9_87320.RData full_cart_5000_0.9_22306.RData full_mars_5000_0.95_43307.RData full_mars_100_0.95_99526.RData full_cart_1000_0.9_45399.RData full_mars_500_0.9_59521.RData full_cart_100_0.9_72611.RData full_lm_500_0.9_25305.RData full_mars_100_0.9_32618.RData full_cart_5000_0.9_12204.RData full_lm_1000_0.9_25305.RData full_cart_500_0.95_90597.RData full_lm_1000_0.9_99526.RData full_mars_100_0.9_59521.RData full_mars_5000_0.95_25173.RData full_cart_1000_0.95_90597.RData full_cart_500_0.9_87320.RData full_mars_1000_0.95_87320.RData full_lm_100_0.9_19242.RData full_lm_5000_0.95_96812.RData full_lm_1000_0.9_59521.RData full_mars_500_0.9_13903.RData full_cart_5000_0.9_87320.RData full_mars_1000_0.95_25173.RData full_mars_5000_0.9_43307.RData full_cart_5000_0.95_45399.RData full_mars_5000_0.9_8229.RData full_mars_5000_0.95_87320.RData full_cart_1000_0.9_72611.RData full_cart_1000_0.95_25173.RData full_lm_100_0.95_25173.RData full_lm_1000_0.9_6519.RData full_cart_100_0.9_45399.RData full_mars_1000_0.9_25305.RData full_cart_5000_0.9_90597.RData full_cart_1000_0.95_11571.RData full_cart_500_0.9_25173.RData full_lm_100_0.95_13903.RData full_cart_1000_0.95_8229.RData full_lm_1000_0.95_45399.RData full_cart_1000_0.9_81580.RData full_mars_1000_0.9_43809.RData full_lm_5000_0.95_32618.RData full_cart_5000_0.9_96812.RData full_lm_5000_0.95_69586.RData full_cart_5000_0.95_32618.RData full_mars_500_0.9_90597.RData full_mars_1000_0.9_81580.RData full_mars_1000_0.9_59521.RData full_mars_100_0.95_92490.RData full_mars_500_0.95_24388.RData full_lm_100_0.9_92490.RData full_mars_5000_0.9_69586.RData full_mars_5000_0.9_36244.RData full_mars_5000_0.95_92199.RData full_lm_1000_0.9_11571.RData full_mars_500_0.95_43809.RData full_lm_1000_0.9_45399.RData full_cart_1000_0.95_45399.RData full_mars_100_0.9_19242.RData full_mars_100_0.95_12204.RData full_cart_100_0.9_43809.RData full_cart_1000_0.9_96812.RData full_lm_500_0.9_92490.RData full_cart_1000_0.95_87320.RData full_mars_100_0.9_22306.RData full_lm_5000_0.95_43809.RData full_mars_100_0.95_25173.RData full_cart_100_0.9_92490.RData full_mars_5000_0.9_43809.RData full_mars_100_0.95_36244.RData full_lm_1000_0.9_72611.RData full_lm_5000_0.95_81580.RData full_mars_500_0.9_12204.RData full_cart_500_0.9_81580.RData full_mars_5000_0.95_8229.RData full_cart_5000_0.95_43809.RData full_lm_5000_0.95_90597.RData full_lm_1000_0.9_92199.RData full_lm_100_0.95_81580.RData full_mars_500_0.95_12204.RData full_mars_1000_0.9_12204.RData full_cart_100_0.95_45399.RData full_lm_5000_0.95_92490.RData full_mars_500_0.95_81580.RData full_lm_500_0.9_36244.RData full_lm_100_0.95_99526.RData full_lm_500_0.9_43307.RData full_cart_100_0.9_96812.RData full_lm_5000_0.9_6519.RData full_mars_500_0.9_36244.RData full_cart_1000_0.95_22306.RData full_lm_500_0.9_43809.RData full_mars_5000_0.9_25305.RData full_cart_5000_0.9_6519.RData full_cart_5000_0.95_69586.RData full_cart_5000_0.95_92490.RData full_mars_100_0.95_22306.RData full_lm_100_0.9_69586.RData full_lm_5000_0.95_45399.RData full_lm_500_0.9_59521.RData full_lm_1000_0.9_43809.RData full_mars_5000_0.95_59521.RData full_lm_100_0.9_99526.RData full_mars_5000_0.9_13903.RData full_mars_1000_0.95_32618.RData full_lm_1000_0.95_90597.RData full_mars_100_0.9_43809.RData

full_mars_500_0.95_11571.RData: full_mars_500_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_11571.R'

full_cart_100_0.95_87320.RData: full_cart_100_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_87320.R'

full_mars_100_0.9_12204.RData: full_mars_100_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_12204.R'

full_cart_100_0.9_92199.RData: full_cart_100_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_92199.R'

full_cart_5000_0.95_81580.RData: full_cart_5000_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_81580.R'

full_lm_5000_0.9_90597.RData: full_lm_5000_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_90597.R'

full_mars_1000_0.95_96812.RData: full_mars_1000_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_96812.R'

full_cart_500_0.9_19242.RData: full_cart_500_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_19242.R'

full_lm_500_0.95_11571.RData: full_lm_500_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_11571.R'

full_mars_1000_0.95_72611.RData: full_mars_1000_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_72611.R'

full_mars_1000_0.9_6519.RData: full_mars_1000_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_6519.R'

full_mars_500_0.9_99526.RData: full_mars_500_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_99526.R'

full_cart_5000_0.95_11571.RData: full_cart_5000_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_11571.R'

full_lm_500_0.95_87320.RData: full_lm_500_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_87320.R'

full_cart_500_0.9_36244.RData: full_cart_500_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_36244.R'

full_cart_1000_0.95_32618.RData: full_cart_1000_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_32618.R'

full_cart_100_0.95_19242.RData: full_cart_100_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_19242.R'

full_cart_500_0.95_69586.RData: full_cart_500_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_69586.R'

full_lm_1000_0.9_12204.RData: full_lm_1000_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_12204.R'

full_lm_500_0.9_19242.RData: full_lm_500_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_19242.R'

full_mars_500_0.95_87320.RData: full_mars_500_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_87320.R'

full_mars_5000_0.9_25173.RData: full_mars_5000_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_25173.R'

full_lm_1000_0.95_43809.RData: full_lm_1000_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_43809.R'

full_cart_500_0.95_87320.RData: full_cart_500_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_87320.R'

full_lm_500_0.95_19242.RData: full_lm_500_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_19242.R'

full_mars_1000_0.95_43809.RData: full_mars_1000_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_43809.R'

full_mars_1000_0.95_92490.RData: full_mars_1000_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_92490.R'

full_mars_1000_0.95_6519.RData: full_mars_1000_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_6519.R'

full_cart_500_0.9_13903.RData: full_cart_500_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_13903.R'

full_lm_500_0.9_69586.RData: full_lm_500_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_69586.R'

full_cart_1000_0.9_11571.RData: full_cart_1000_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_11571.R'

full_lm_1000_0.95_6519.RData: full_lm_1000_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_6519.R'

full_cart_500_0.95_19242.RData: full_cart_500_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_19242.R'

full_lm_1000_0.95_25173.RData: full_lm_1000_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_25173.R'

full_cart_500_0.95_81580.RData: full_cart_500_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_81580.R'

full_mars_100_0.95_72611.RData: full_mars_100_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_72611.R'

full_lm_1000_0.95_36244.RData: full_lm_1000_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_36244.R'

full_cart_100_0.95_92490.RData: full_cart_100_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_92490.R'

full_mars_500_0.9_11571.RData: full_mars_500_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_11571.R'

full_mars_500_0.9_43307.RData: full_mars_500_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_43307.R'

full_lm_1000_0.95_87320.RData: full_lm_1000_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_87320.R'

full_mars_500_0.95_43307.RData: full_mars_500_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_43307.R'

full_lm_100_0.9_59521.RData: full_lm_100_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_59521.R'

full_lm_500_0.95_59521.RData: full_lm_500_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_59521.R'

full_lm_500_0.95_90597.RData: full_lm_500_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_90597.R'

full_cart_100_0.95_81580.RData: full_cart_100_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_81580.R'

full_cart_100_0.9_11571.RData: full_cart_100_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_11571.R'

full_mars_5000_0.95_32618.RData: full_mars_5000_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_32618.R'

full_lm_100_0.95_25305.RData: full_lm_100_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_25305.R'

full_lm_1000_0.95_81580.RData: full_lm_1000_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_81580.R'

full_lm_5000_0.9_8229.RData: full_lm_5000_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_8229.R'

full_mars_1000_0.9_11571.RData: full_mars_1000_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_11571.R'

full_cart_1000_0.95_43307.RData: full_cart_1000_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_43307.R'

full_mars_1000_0.95_25305.RData: full_mars_1000_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_25305.R'

full_lm_500_0.95_12204.RData: full_lm_500_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_12204.R'

full_lm_5000_0.9_22306.RData: full_lm_5000_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_22306.R'

full_cart_100_0.95_96812.RData: full_cart_100_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_96812.R'

full_lm_5000_0.9_43307.RData: full_lm_5000_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_43307.R'

full_mars_5000_0.9_90597.RData: full_mars_5000_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_90597.R'

full_lm_100_0.95_72611.RData: full_lm_100_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_72611.R'

full_mars_100_0.95_6519.RData: full_mars_100_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_6519.R'

full_lm_100_0.95_92199.RData: full_lm_100_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_92199.R'

full_lm_100_0.9_8229.RData: full_lm_100_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_8229.R'

full_cart_500_0.95_36244.RData: full_cart_500_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_36244.R'

full_cart_500_0.9_43307.RData: full_cart_500_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_43307.R'

full_mars_500_0.95_25173.RData: full_mars_500_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_25173.R'

full_lm_5000_0.95_12204.RData: full_lm_5000_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_12204.R'

full_mars_100_0.9_25305.RData: full_mars_100_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_25305.R'

full_mars_5000_0.9_87320.RData: full_mars_5000_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_87320.R'

full_mars_1000_0.9_72611.RData: full_mars_1000_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_72611.R'

full_lm_5000_0.9_24388.RData: full_lm_5000_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_24388.R'

full_cart_500_0.95_22306.RData: full_cart_500_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_22306.R'

full_cart_1000_0.9_6519.RData: full_cart_1000_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_6519.R'

full_lm_5000_0.9_32618.RData: full_lm_5000_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_32618.R'

full_cart_500_0.9_22306.RData: full_cart_500_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_22306.R'

full_cart_500_0.9_96812.RData: full_cart_500_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_96812.R'

full_cart_1000_0.95_12204.RData: full_cart_1000_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_12204.R'

full_lm_100_0.9_81580.RData: full_lm_100_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_81580.R'

full_cart_5000_0.95_8229.RData: full_cart_5000_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_8229.R'

full_cart_100_0.9_8229.RData: full_cart_100_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_8229.R'

full_lm_1000_0.9_92490.RData: full_lm_1000_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_92490.R'

full_mars_100_0.9_81580.RData: full_mars_100_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_81580.R'

full_mars_100_0.9_8229.RData: full_mars_100_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_8229.R'

full_mars_100_0.9_13903.RData: full_mars_100_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_13903.R'

full_cart_100_0.9_69586.RData: full_cart_100_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_69586.R'

full_cart_100_0.95_6519.RData: full_cart_100_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_6519.R'

full_cart_500_0.95_45399.RData: full_cart_500_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_45399.R'

full_mars_1000_0.95_99526.RData: full_mars_1000_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_99526.R'

full_mars_1000_0.95_36244.RData: full_mars_1000_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_36244.R'

full_lm_100_0.95_96812.RData: full_lm_100_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_96812.R'

full_cart_5000_0.9_36244.RData: full_cart_5000_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_36244.R'

full_lm_100_0.95_19242.RData: full_lm_100_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_19242.R'

full_mars_100_0.9_92490.RData: full_mars_100_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_92490.R'

full_mars_1000_0.95_19242.RData: full_mars_1000_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_19242.R'

full_mars_100_0.9_90597.RData: full_mars_100_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_90597.R'

full_mars_1000_0.95_12204.RData: full_mars_1000_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_12204.R'

full_cart_500_0.95_6519.RData: full_cart_500_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_6519.R'

full_cart_500_0.95_11571.RData: full_cart_500_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_11571.R'

full_cart_100_0.9_59521.RData: full_cart_100_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_59521.R'

full_mars_1000_0.95_43307.RData: full_mars_1000_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_43307.R'

full_cart_500_0.9_69586.RData: full_cart_500_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_69586.R'

full_mars_500_0.9_69586.RData: full_mars_500_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_69586.R'

full_lm_1000_0.9_90597.RData: full_lm_1000_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_90597.R'

full_mars_500_0.95_19242.RData: full_mars_500_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_19242.R'

full_mars_500_0.95_13903.RData: full_mars_500_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_13903.R'

full_mars_500_0.95_96812.RData: full_mars_500_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_96812.R'

full_mars_1000_0.9_25173.RData: full_mars_1000_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_25173.R'

full_lm_5000_0.9_25305.RData: full_lm_5000_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_25305.R'

full_mars_500_0.9_87320.RData: full_mars_500_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_87320.R'

full_lm_100_0.9_43809.RData: full_lm_100_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_43809.R'

full_cart_500_0.95_12204.RData: full_cart_500_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_12204.R'

full_cart_500_0.95_25305.RData: full_cart_500_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_25305.R'

full_cart_100_0.95_69586.RData: full_cart_100_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_69586.R'

full_mars_1000_0.9_99526.RData: full_mars_1000_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_99526.R'

full_lm_5000_0.9_59521.RData: full_lm_5000_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_59521.R'

full_mars_100_0.9_6519.RData: full_mars_100_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_6519.R'

full_lm_5000_0.9_92490.RData: full_lm_5000_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_92490.R'

full_lm_500_0.95_13903.RData: full_lm_500_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_13903.R'

full_cart_100_0.95_43307.RData: full_cart_100_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_43307.R'

full_lm_500_0.9_22306.RData: full_lm_500_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_22306.R'

full_lm_1000_0.95_92199.RData: full_lm_1000_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_92199.R'

full_mars_100_0.9_45399.RData: full_mars_100_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_45399.R'

full_mars_1000_0.9_8229.RData: full_mars_1000_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_8229.R'

full_cart_1000_0.9_59521.RData: full_cart_1000_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_59521.R'

full_mars_5000_0.9_81580.RData: full_mars_5000_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_81580.R'

full_lm_500_0.9_32618.RData: full_lm_500_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_32618.R'

full_mars_1000_0.95_69586.RData: full_mars_1000_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_69586.R'

full_lm_5000_0.9_43809.RData: full_lm_5000_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_43809.R'

full_mars_100_0.95_69586.RData: full_mars_100_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_69586.R'

full_lm_500_0.95_8229.RData: full_lm_500_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_8229.R'

full_cart_500_0.9_72611.RData: full_cart_500_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_72611.R'

full_mars_5000_0.9_99526.RData: full_mars_5000_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_99526.R'

full_cart_500_0.9_12204.RData: full_cart_500_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_12204.R'

full_cart_100_0.95_99526.RData: full_cart_100_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_99526.R'

full_lm_500_0.95_43307.RData: full_lm_500_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_43307.R'

full_lm_1000_0.95_43307.RData: full_lm_1000_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_43307.R'

full_lm_1000_0.9_19242.RData: full_lm_1000_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_19242.R'

full_lm_500_0.95_96812.RData: full_lm_500_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_96812.R'

full_lm_5000_0.95_43307.RData: full_lm_5000_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_43307.R'

full_cart_1000_0.9_92199.RData: full_cart_1000_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_92199.R'

full_lm_5000_0.95_25173.RData: full_lm_5000_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_25173.R'

full_cart_1000_0.95_13903.RData: full_cart_1000_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_13903.R'

full_mars_100_0.9_87320.RData: full_mars_100_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_87320.R'

full_cart_5000_0.9_81580.RData: full_cart_5000_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_81580.R'

full_mars_500_0.9_24388.RData: full_mars_500_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_24388.R'

full_mars_500_0.95_92199.RData: full_mars_500_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_92199.R'

full_mars_500_0.95_6519.RData: full_mars_500_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_6519.R'

full_mars_500_0.9_45399.RData: full_mars_500_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_45399.R'

full_mars_500_0.95_22306.RData: full_mars_500_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_22306.R'

full_cart_500_0.9_45399.RData: full_cart_500_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_45399.R'

full_lm_100_0.95_32618.RData: full_lm_100_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_32618.R'

full_cart_500_0.9_90597.RData: full_cart_500_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_90597.R'

full_cart_1000_0.9_92490.RData: full_cart_1000_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_92490.R'

full_cart_100_0.95_12204.RData: full_cart_100_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_12204.R'

full_mars_1000_0.95_45399.RData: full_mars_1000_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_45399.R'

full_cart_100_0.9_6519.RData: full_cart_100_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_6519.R'

full_mars_1000_0.9_92199.RData: full_mars_1000_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_92199.R'

full_lm_1000_0.95_8229.RData: full_lm_1000_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_8229.R'

full_cart_1000_0.9_43809.RData: full_cart_1000_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_43809.R'

full_lm_5000_0.95_6519.RData: full_lm_5000_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_6519.R'

full_cart_500_0.95_25173.RData: full_cart_500_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_25173.R'

full_cart_100_0.95_36244.RData: full_cart_100_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_36244.R'

full_lm_5000_0.95_24388.RData: full_lm_5000_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_24388.R'

full_mars_5000_0.95_22306.RData: full_mars_5000_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_22306.R'

full_mars_500_0.9_96812.RData: full_mars_500_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_96812.R'

full_mars_5000_0.95_99526.RData: full_mars_5000_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_99526.R'

full_cart_5000_0.95_36244.RData: full_cart_5000_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_36244.R'

full_mars_500_0.95_90597.RData: full_mars_500_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_90597.R'

full_cart_5000_0.9_13903.RData: full_cart_5000_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_13903.R'

full_cart_100_0.95_24388.RData: full_cart_100_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_24388.R'

full_cart_100_0.95_43809.RData: full_cart_100_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_43809.R'

full_lm_1000_0.9_22306.RData: full_lm_1000_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_22306.R'

full_lm_100_0.95_24388.RData: full_lm_100_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_24388.R'

full_cart_100_0.95_13903.RData: full_cart_100_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_13903.R'

full_mars_5000_0.9_6519.RData: full_mars_5000_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_6519.R'

full_cart_500_0.95_92490.RData: full_cart_500_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_92490.R'

full_mars_5000_0.9_59521.RData: full_mars_5000_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_59521.R'

full_mars_5000_0.9_92490.RData: full_mars_5000_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_92490.R'

full_lm_1000_0.95_92490.RData: full_lm_1000_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_92490.R'

full_cart_1000_0.9_36244.RData: full_cart_1000_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_36244.R'

full_lm_1000_0.95_32618.RData: full_lm_1000_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_32618.R'

full_lm_500_0.95_32618.RData: full_lm_500_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_32618.R'

full_mars_100_0.95_24388.RData: full_mars_100_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_24388.R'

full_lm_1000_0.9_81580.RData: full_lm_1000_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_81580.R'

full_cart_1000_0.95_81580.RData: full_cart_1000_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_81580.R'

full_mars_1000_0.9_69586.RData: full_mars_1000_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_69586.R'

full_lm_1000_0.95_99526.RData: full_lm_1000_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_99526.R'

full_lm_100_0.9_12204.RData: full_lm_100_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_12204.R'

full_cart_1000_0.95_92490.RData: full_cart_1000_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_92490.R'

full_mars_5000_0.9_72611.RData: full_mars_5000_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_72611.R'

full_lm_100_0.95_69586.RData: full_lm_100_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_69586.R'

full_cart_100_0.95_25305.RData: full_cart_100_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_25305.R'

full_mars_1000_0.9_19242.RData: full_mars_1000_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_19242.R'

full_mars_1000_0.9_87320.RData: full_mars_1000_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_87320.R'

full_mars_500_0.95_92490.RData: full_mars_500_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_92490.R'

full_lm_100_0.9_72611.RData: full_lm_100_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_72611.R'

full_lm_1000_0.95_22306.RData: full_lm_1000_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_22306.R'

full_mars_5000_0.9_11571.RData: full_mars_5000_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_11571.R'

full_cart_500_0.9_25305.RData: full_cart_500_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_25305.R'

full_mars_5000_0.9_96812.RData: full_mars_5000_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_96812.R'

full_lm_1000_0.9_87320.RData: full_lm_1000_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_87320.R'

full_lm_100_0.95_59521.RData: full_lm_100_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_59521.R'

full_lm_100_0.9_32618.RData: full_lm_100_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_32618.R'

full_lm_500_0.95_45399.RData: full_lm_500_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_45399.R'

full_cart_1000_0.95_92199.RData: full_cart_1000_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_92199.R'

full_lm_100_0.9_96812.RData: full_lm_100_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_96812.R'

full_mars_1000_0.9_43307.RData: full_mars_1000_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_43307.R'

full_cart_1000_0.9_13903.RData: full_cart_1000_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_13903.R'

full_cart_1000_0.9_24388.RData: full_cart_1000_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_24388.R'

full_mars_100_0.95_13903.RData: full_mars_100_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_13903.R'

full_cart_5000_0.95_96812.RData: full_cart_5000_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_96812.R'

full_lm_1000_0.9_24388.RData: full_lm_1000_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_24388.R'

full_lm_100_0.9_11571.RData: full_lm_100_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_11571.R'

full_lm_5000_0.9_19242.RData: full_lm_5000_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_19242.R'

full_lm_5000_0.9_25173.RData: full_lm_5000_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_25173.R'

full_mars_1000_0.9_45399.RData: full_mars_1000_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_45399.R'

full_cart_1000_0.9_99526.RData: full_cart_1000_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_99526.R'

full_cart_100_0.9_12204.RData: full_cart_100_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_12204.R'

full_mars_1000_0.9_13903.RData: full_mars_1000_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_13903.R'

full_cart_500_0.95_24388.RData: full_cart_500_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_24388.R'

full_lm_5000_0.9_72611.RData: full_lm_5000_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_72611.R'

full_mars_5000_0.9_12204.RData: full_mars_5000_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_12204.R'

full_mars_100_0.9_11571.RData: full_mars_100_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_11571.R'

full_cart_5000_0.9_24388.RData: full_cart_5000_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_24388.R'

full_cart_5000_0.95_92199.RData: full_cart_5000_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_92199.R'

full_lm_100_0.9_87320.RData: full_lm_100_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_87320.R'

full_lm_1000_0.95_96812.RData: full_lm_1000_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_96812.R'

full_lm_100_0.95_36244.RData: full_lm_100_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_36244.R'

full_mars_5000_0.9_19242.RData: full_mars_5000_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_19242.R'

full_cart_5000_0.9_92490.RData: full_cart_5000_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_92490.R'

full_lm_5000_0.9_13903.RData: full_lm_5000_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_13903.R'

full_cart_1000_0.95_25305.RData: full_cart_1000_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_25305.R'

full_lm_500_0.9_96812.RData: full_lm_500_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_96812.R'

full_lm_100_0.9_25305.RData: full_lm_100_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_25305.R'

full_mars_500_0.95_45399.RData: full_mars_500_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_45399.R'

full_mars_100_0.95_32618.RData: full_mars_100_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_32618.R'

full_mars_5000_0.9_92199.RData: full_mars_5000_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_92199.R'

full_mars_1000_0.95_81580.RData: full_mars_1000_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_81580.R'

full_lm_1000_0.9_13903.RData: full_lm_1000_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_13903.R'

full_cart_5000_0.95_22306.RData: full_cart_5000_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_22306.R'

full_cart_5000_0.9_43809.RData: full_cart_5000_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_43809.R'

full_lm_100_0.9_90597.RData: full_lm_100_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_90597.R'

full_cart_5000_0.9_45399.RData: full_cart_5000_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_45399.R'

full_lm_500_0.95_22306.RData: full_lm_500_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_22306.R'

full_cart_5000_0.9_72611.RData: full_cart_5000_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_72611.R'

full_mars_500_0.95_8229.RData: full_mars_500_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_8229.R'

full_cart_5000_0.95_6519.RData: full_cart_5000_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_6519.R'

full_cart_5000_0.9_43307.RData: full_cart_5000_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_43307.R'

full_lm_1000_0.95_69586.RData: full_lm_1000_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_69586.R'

full_mars_5000_0.95_25305.RData: full_mars_5000_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_25305.R'

full_mars_500_0.95_59521.RData: full_mars_500_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_59521.R'

full_mars_500_0.9_32618.RData: full_mars_500_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_32618.R'

full_mars_500_0.95_72611.RData: full_mars_500_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_72611.R'

full_mars_5000_0.9_24388.RData: full_mars_5000_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_24388.R'

full_lm_500_0.95_92199.RData: full_lm_500_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_92199.R'

full_lm_5000_0.9_36244.RData: full_lm_5000_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_36244.R'

full_lm_5000_0.9_81580.RData: full_lm_5000_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_81580.R'

full_lm_5000_0.95_72611.RData: full_lm_5000_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_72611.R'

full_cart_100_0.9_99526.RData: full_cart_100_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_99526.R'

full_cart_1000_0.95_24388.RData: full_cart_1000_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_24388.R'

full_mars_5000_0.95_11571.RData: full_mars_5000_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_11571.R'

full_lm_100_0.9_92199.RData: full_lm_100_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_92199.R'

full_lm_500_0.95_6519.RData: full_lm_500_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_6519.R'

full_lm_100_0.9_13903.RData: full_lm_100_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_13903.R'

full_mars_5000_0.95_81580.RData: full_mars_5000_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_81580.R'

full_cart_100_0.9_13903.RData: full_cart_100_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_13903.R'

full_cart_5000_0.95_19242.RData: full_cart_5000_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_19242.R'

full_cart_500_0.95_92199.RData: full_cart_500_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_92199.R'

full_lm_5000_0.9_87320.RData: full_lm_5000_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_87320.R'

full_mars_500_0.9_25305.RData: full_mars_500_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_25305.R'

full_lm_1000_0.9_36244.RData: full_lm_1000_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_36244.R'

full_lm_100_0.9_22306.RData: full_lm_100_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_22306.R'

full_mars_1000_0.9_24388.RData: full_mars_1000_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_24388.R'

full_lm_500_0.9_90597.RData: full_lm_500_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_90597.R'

full_cart_5000_0.95_24388.RData: full_cart_5000_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_24388.R'

full_mars_1000_0.95_59521.RData: full_mars_1000_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_59521.R'

full_cart_500_0.9_24388.RData: full_cart_500_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_24388.R'

full_mars_100_0.95_59521.RData: full_mars_100_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_59521.R'

full_lm_1000_0.95_19242.RData: full_lm_1000_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_19242.R'

full_lm_5000_0.9_12204.RData: full_lm_5000_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_12204.R'

full_cart_5000_0.9_59521.RData: full_cart_5000_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_59521.R'

full_cart_1000_0.95_96812.RData: full_cart_1000_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_96812.R'

full_lm_5000_0.9_45399.RData: full_lm_5000_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_45399.R'

full_mars_100_0.9_99526.RData: full_mars_100_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_99526.R'

full_cart_500_0.95_99526.RData: full_cart_500_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_99526.R'

full_mars_100_0.95_96812.RData: full_mars_100_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_96812.R'

full_cart_5000_0.9_8229.RData: full_cart_5000_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_8229.R'

full_mars_100_0.9_24388.RData: full_mars_100_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_24388.R'

full_mars_5000_0.95_92490.RData: full_mars_5000_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_92490.R'

full_mars_100_0.95_19242.RData: full_mars_100_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_19242.R'

full_cart_100_0.9_81580.RData: full_cart_100_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_81580.R'

full_cart_100_0.95_8229.RData: full_cart_100_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_8229.R'

full_mars_100_0.9_96812.RData: full_mars_100_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_96812.R'

full_cart_1000_0.95_69586.RData: full_cart_1000_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_69586.R'

full_lm_500_0.9_87320.RData: full_lm_500_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_87320.R'

full_cart_100_0.95_25173.RData: full_cart_100_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_25173.R'

full_mars_500_0.95_36244.RData: full_mars_500_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_36244.R'

full_cart_500_0.9_8229.RData: full_cart_500_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_8229.R'

full_lm_5000_0.95_36244.RData: full_lm_5000_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_36244.R'

full_mars_5000_0.95_24388.RData: full_mars_5000_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_24388.R'

full_lm_500_0.95_99526.RData: full_lm_500_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_99526.R'

full_cart_1000_0.95_59521.RData: full_cart_1000_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_59521.R'

full_cart_100_0.9_90597.RData: full_cart_100_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_90597.R'

full_cart_1000_0.95_19242.RData: full_cart_1000_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_19242.R'

full_cart_5000_0.95_72611.RData: full_cart_5000_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_72611.R'

full_cart_1000_0.9_90597.RData: full_cart_1000_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_90597.R'

full_lm_5000_0.9_69586.RData: full_lm_5000_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_69586.R'

full_cart_1000_0.9_25305.RData: full_cart_1000_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_25305.R'

full_lm_5000_0.95_13903.RData: full_lm_5000_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_13903.R'

full_cart_500_0.9_59521.RData: full_cart_500_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_59521.R'

full_cart_100_0.95_32618.RData: full_cart_100_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_32618.R'

full_lm_1000_0.95_12204.RData: full_lm_1000_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_12204.R'

full_lm_5000_0.9_92199.RData: full_lm_5000_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_92199.R'

full_mars_5000_0.95_69586.RData: full_mars_5000_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_69586.R'

full_lm_5000_0.95_92199.RData: full_lm_5000_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_92199.R'

full_mars_100_0.95_11571.RData: full_mars_100_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_11571.R'

full_mars_100_0.9_25173.RData: full_mars_100_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_25173.R'

full_mars_1000_0.95_8229.RData: full_mars_1000_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_8229.R'

full_mars_100_0.95_90597.RData: full_mars_100_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_90597.R'

full_lm_500_0.9_6519.RData: full_lm_500_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_6519.R'

full_lm_100_0.95_92490.RData: full_lm_100_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_92490.R'

full_lm_100_0.95_45399.RData: full_lm_100_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_45399.R'

full_cart_500_0.9_92199.RData: full_cart_500_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_92199.R'

full_mars_1000_0.9_96812.RData: full_mars_1000_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_96812.R'

full_cart_100_0.9_22306.RData: full_cart_100_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_22306.R'

full_cart_100_0.9_87320.RData: full_cart_100_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_87320.R'

full_cart_5000_0.95_25305.RData: full_cart_5000_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_25305.R'

full_lm_500_0.95_81580.RData: full_lm_500_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_81580.R'

full_mars_1000_0.95_22306.RData: full_mars_1000_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_22306.R'

full_lm_5000_0.95_25305.RData: full_lm_5000_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_25305.R'

full_cart_1000_0.9_8229.RData: full_cart_1000_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_8229.R'

full_mars_500_0.9_72611.RData: full_mars_500_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_72611.R'

full_lm_500_0.9_12204.RData: full_lm_500_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_12204.R'

full_lm_5000_0.9_11571.RData: full_lm_5000_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_11571.R'

full_mars_500_0.95_25305.RData: full_mars_500_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_25305.R'

full_cart_100_0.95_72611.RData: full_cart_100_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_72611.R'

full_cart_500_0.9_92490.RData: full_cart_500_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_92490.R'

full_mars_500_0.95_69586.RData: full_mars_500_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_69586.R'

full_lm_500_0.9_99526.RData: full_lm_500_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_99526.R'

full_mars_5000_0.95_90597.RData: full_mars_5000_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_90597.R'

full_lm_500_0.9_92199.RData: full_lm_500_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_92199.R'

full_cart_5000_0.95_12204.RData: full_cart_5000_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_12204.R'

full_mars_500_0.95_99526.RData: full_mars_500_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_99526.R'

full_cart_1000_0.95_72611.RData: full_cart_1000_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_72611.R'

full_cart_500_0.9_43809.RData: full_cart_500_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_43809.R'

full_lm_1000_0.9_32618.RData: full_lm_1000_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_32618.R'

full_lm_500_0.9_11571.RData: full_lm_500_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_11571.R'

full_cart_100_0.95_90597.RData: full_cart_100_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_90597.R'

full_lm_100_0.95_22306.RData: full_lm_100_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_22306.R'

full_cart_500_0.95_59521.RData: full_cart_500_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_59521.R'

full_cart_100_0.9_25305.RData: full_cart_100_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_25305.R'

full_lm_100_0.9_6519.RData: full_lm_100_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_6519.R'

full_cart_5000_0.95_59521.RData: full_cart_5000_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_59521.R'

full_mars_500_0.9_22306.RData: full_mars_500_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_22306.R'

full_mars_5000_0.95_36244.RData: full_mars_5000_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_36244.R'

full_cart_500_0.9_6519.RData: full_cart_500_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_6519.R'

full_lm_5000_0.95_11571.RData: full_lm_5000_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_11571.R'

full_mars_500_0.9_92199.RData: full_mars_500_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_92199.R'

full_lm_100_0.95_8229.RData: full_lm_100_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_8229.R'

full_mars_500_0.9_19242.RData: full_mars_500_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_19242.R'

full_cart_5000_0.95_87320.RData: full_cart_5000_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_87320.R'

full_mars_500_0.9_92490.RData: full_mars_500_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_92490.R'

full_mars_100_0.95_25305.RData: full_mars_100_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_25305.R'

full_mars_1000_0.9_32618.RData: full_mars_1000_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_32618.R'

full_mars_100_0.95_43307.RData: full_mars_100_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_43307.R'

full_cart_5000_0.9_32618.RData: full_cart_5000_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_32618.R'

full_lm_500_0.9_13903.RData: full_lm_500_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_13903.R'

full_lm_100_0.95_11571.RData: full_lm_100_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_11571.R'

full_cart_100_0.95_22306.RData: full_cart_100_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_22306.R'

full_cart_100_0.95_11571.RData: full_cart_100_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_11571.R'

full_cart_500_0.9_32618.RData: full_cart_500_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_32618.R'

full_mars_5000_0.95_45399.RData: full_mars_5000_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_45399.R'

full_cart_100_0.9_25173.RData: full_cart_100_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_25173.R'

full_lm_5000_0.95_59521.RData: full_lm_5000_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_59521.R'

full_mars_100_0.95_92199.RData: full_mars_100_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_92199.R'

full_mars_5000_0.9_22306.RData: full_mars_5000_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_22306.R'

full_cart_500_0.95_43809.RData: full_cart_500_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_43809.R'

full_cart_500_0.95_43307.RData: full_cart_500_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_43307.R'

full_lm_500_0.9_45399.RData: full_lm_500_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_45399.R'

full_mars_5000_0.95_96812.RData: full_mars_5000_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_96812.R'

full_cart_500_0.95_32618.RData: full_cart_500_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_32618.R'

full_lm_100_0.95_12204.RData: full_lm_100_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_12204.R'

full_lm_5000_0.95_19242.RData: full_lm_5000_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_19242.R'

full_cart_5000_0.9_92199.RData: full_cart_5000_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_92199.R'

full_cart_100_0.9_36244.RData: full_cart_100_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_36244.R'

full_cart_5000_0.9_25305.RData: full_cart_5000_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_25305.R'

full_mars_1000_0.95_11571.RData: full_mars_1000_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_11571.R'

full_cart_100_0.9_24388.RData: full_cart_100_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_24388.R'

full_mars_5000_0.95_12204.RData: full_mars_5000_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_12204.R'

full_mars_1000_0.9_22306.RData: full_mars_1000_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_22306.R'

full_mars_5000_0.95_6519.RData: full_mars_5000_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_6519.R'

full_cart_5000_0.95_25173.RData: full_cart_5000_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_25173.R'

full_mars_1000_0.95_92199.RData: full_mars_1000_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_92199.R'

full_cart_5000_0.9_99526.RData: full_cart_5000_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_99526.R'

full_cart_1000_0.95_99526.RData: full_cart_1000_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_99526.R'

full_mars_1000_0.95_13903.RData: full_mars_1000_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_13903.R'

full_lm_1000_0.95_11571.RData: full_lm_1000_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_11571.R'

full_cart_5000_0.9_69586.RData: full_cart_5000_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_69586.R'

full_lm_100_0.95_87320.RData: full_lm_100_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_87320.R'

full_lm_100_0.95_90597.RData: full_lm_100_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_90597.R'

full_lm_1000_0.9_25173.RData: full_lm_1000_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_25173.R'

full_mars_500_0.95_32618.RData: full_mars_500_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_32618.R'

full_mars_100_0.9_36244.RData: full_mars_100_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_36244.R'

full_lm_500_0.9_8229.RData: full_lm_500_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_8229.R'

full_cart_100_0.9_19242.RData: full_cart_100_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_19242.R'

full_cart_100_0.95_92199.RData: full_cart_100_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_92199.R'

full_cart_500_0.95_13903.RData: full_cart_500_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_13903.R'

full_cart_5000_0.9_19242.RData: full_cart_5000_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_19242.R'

full_lm_100_0.95_43307.RData: full_lm_100_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_43307.R'

full_mars_100_0.95_45399.RData: full_mars_100_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_45399.R'

full_mars_100_0.95_87320.RData: full_mars_100_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_87320.R'

full_cart_500_0.9_99526.RData: full_cart_500_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_99526.R'

full_lm_500_0.95_43809.RData: full_lm_500_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_43809.R'

full_mars_500_0.9_81580.RData: full_mars_500_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_81580.R'

full_mars_1000_0.9_36244.RData: full_mars_1000_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_36244.R'

full_lm_1000_0.9_69586.RData: full_lm_1000_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_69586.R'

full_lm_1000_0.9_43307.RData: full_lm_1000_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_43307.R'

full_cart_1000_0.95_36244.RData: full_cart_1000_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_36244.R'

full_mars_1000_0.9_92490.RData: full_mars_1000_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_92490.R'

full_cart_1000_0.9_22306.RData: full_cart_1000_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_22306.R'

full_lm_500_0.95_24388.RData: full_lm_500_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_24388.R'

full_mars_100_0.9_92199.RData: full_mars_100_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_92199.R'

full_lm_500_0.95_36244.RData: full_lm_500_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_36244.R'

full_cart_1000_0.95_6519.RData: full_cart_1000_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_6519.R'

full_cart_1000_0.95_43809.RData: full_cart_1000_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_43809.R'

full_mars_100_0.95_43809.RData: full_mars_100_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_43809.R'

full_lm_500_0.95_92490.RData: full_lm_500_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_92490.R'

full_lm_100_0.95_6519.RData: full_lm_100_0.95_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_6519.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_6519.R'

full_cart_5000_0.95_13903.RData: full_cart_5000_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_13903.R'

full_mars_100_0.95_81580.RData: full_mars_100_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_81580.R'

full_mars_5000_0.95_13903.RData: full_mars_5000_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_13903.R'

full_cart_5000_0.95_90597.RData: full_cart_5000_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_90597.R'

full_lm_500_0.9_24388.RData: full_lm_500_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_24388.R'

full_lm_1000_0.9_8229.RData: full_lm_1000_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_8229.R'

full_cart_1000_0.9_12204.RData: full_cart_1000_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_12204.R'

full_mars_5000_0.9_32618.RData: full_mars_5000_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_32618.R'

full_mars_5000_0.95_19242.RData: full_mars_5000_0.95_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_19242.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_19242.R'

full_lm_1000_0.9_96812.RData: full_lm_1000_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_96812.R'

full_lm_1000_0.95_24388.RData: full_lm_1000_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_24388.R'

full_lm_500_0.9_25173.RData: full_lm_500_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_25173.R'

full_lm_1000_0.95_13903.RData: full_lm_1000_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_13903.R'

full_lm_100_0.9_36244.RData: full_lm_100_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_36244.R'

full_cart_5000_0.95_43307.RData: full_cart_5000_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_43307.R'

full_cart_100_0.9_43307.RData: full_cart_100_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_43307.R'

full_lm_5000_0.9_99526.RData: full_lm_5000_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_99526.R'

full_mars_5000_0.9_45399.RData: full_mars_5000_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_45399.R'

full_lm_100_0.9_24388.RData: full_lm_100_0.9_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_24388.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_24388.R'

full_cart_1000_0.9_25173.RData: full_cart_1000_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_25173.R'

full_mars_100_0.95_8229.RData: full_mars_100_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_8229.R'

full_cart_1000_0.9_19242.RData: full_cart_1000_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_19242.R'

full_mars_500_0.9_43809.RData: full_mars_500_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_43809.R'

full_cart_5000_0.9_25173.RData: full_cart_5000_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_25173.R'

full_cart_5000_0.9_11571.RData: full_cart_5000_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_11571.R'

full_lm_500_0.9_81580.RData: full_lm_500_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_81580.R'

full_lm_500_0.95_69586.RData: full_lm_500_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_69586.R'

full_cart_100_0.9_32618.RData: full_cart_100_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_32618.R'

full_lm_500_0.9_72611.RData: full_lm_500_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_72611.R'

full_mars_500_0.9_25173.RData: full_mars_500_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_25173.R'

full_mars_500_0.9_8229.RData: full_mars_500_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_8229.R'

full_lm_5000_0.95_8229.RData: full_lm_5000_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_8229.R'

full_lm_100_0.9_45399.RData: full_lm_100_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_45399.R'

full_mars_5000_0.95_43809.RData: full_mars_5000_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_43809.R'

full_lm_500_0.95_72611.RData: full_lm_500_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_72611.R'

full_lm_1000_0.95_72611.RData: full_lm_1000_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_72611.R'

full_cart_100_0.95_59521.RData: full_cart_100_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_59521.R'

full_mars_1000_0.95_24388.RData: full_mars_1000_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_24388.R'

full_mars_500_0.9_6519.RData: full_mars_500_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_6519.R'

full_cart_1000_0.9_32618.RData: full_cart_1000_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_32618.R'

full_lm_1000_0.95_25305.RData: full_lm_1000_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_25305.R'

full_cart_500_0.95_96812.RData: full_cart_500_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_96812.R'

full_lm_5000_0.95_22306.RData: full_lm_5000_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_22306.R'

full_mars_5000_0.95_72611.RData: full_mars_5000_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_72611.R'

full_cart_500_0.95_8229.RData: full_cart_500_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_8229.R'

full_mars_100_0.9_43307.RData: full_mars_100_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_43307.R'

full_cart_1000_0.9_43307.RData: full_cart_1000_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_43307.R'

full_cart_5000_0.95_99526.RData: full_cart_5000_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_99526.R'

full_lm_100_0.9_43307.RData: full_lm_100_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_43307.R'

full_mars_1000_0.95_90597.RData: full_mars_1000_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_90597.R'

full_lm_5000_0.95_87320.RData: full_lm_5000_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_87320.R'

full_lm_1000_0.95_59521.RData: full_lm_1000_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_59521.R'

full_lm_100_0.9_25173.RData: full_lm_100_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_25173.R'

full_lm_5000_0.95_99526.RData: full_lm_5000_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_99526.R'

full_lm_500_0.95_25173.RData: full_lm_500_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_25173.R'

full_mars_1000_0.9_90597.RData: full_mars_1000_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_90597.R'

full_mars_100_0.9_69586.RData: full_mars_100_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_69586.R'

full_cart_1000_0.9_69586.RData: full_cart_1000_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_69586.R'

full_cart_500_0.9_11571.RData: full_cart_500_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_11571.R'

full_cart_500_0.95_72611.RData: full_cart_500_0.95_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_72611.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_72611.R'

full_lm_100_0.95_43809.RData: full_lm_100_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_43809.R'

full_lm_5000_0.9_96812.RData: full_lm_5000_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_96812.R'

full_lm_500_0.95_25305.RData: full_lm_500_0.95_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.95_25305.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.95_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.95_25305.R'

full_mars_100_0.9_72611.RData: full_mars_100_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_72611.R'

full_cart_1000_0.9_87320.RData: full_cart_1000_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_87320.R'

full_cart_5000_0.9_22306.RData: full_cart_5000_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_22306.R'

full_mars_5000_0.95_43307.RData: full_mars_5000_0.95_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_43307.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_43307.R'

full_mars_100_0.95_99526.RData: full_mars_100_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_99526.R'

full_cart_1000_0.9_45399.RData: full_cart_1000_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_45399.R'

full_mars_500_0.9_59521.RData: full_mars_500_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_59521.R'

full_cart_100_0.9_72611.RData: full_cart_100_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_72611.R'

full_lm_500_0.9_25305.RData: full_lm_500_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_25305.R'

full_mars_100_0.9_32618.RData: full_mars_100_0.9_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_32618.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_32618.R'

full_cart_5000_0.9_12204.RData: full_cart_5000_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_12204.R'

full_lm_1000_0.9_25305.RData: full_lm_1000_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_25305.R'

full_cart_500_0.95_90597.RData: full_cart_500_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.95_90597.R'

full_lm_1000_0.9_99526.RData: full_lm_1000_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_99526.R'

full_mars_100_0.9_59521.RData: full_mars_100_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_59521.R'

full_mars_5000_0.95_25173.RData: full_mars_5000_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_25173.R'

full_cart_1000_0.95_90597.RData: full_cart_1000_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_90597.R'

full_cart_500_0.9_87320.RData: full_cart_500_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_87320.R'

full_mars_1000_0.95_87320.RData: full_mars_1000_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_87320.R'

full_lm_100_0.9_19242.RData: full_lm_100_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_19242.R'

full_lm_5000_0.95_96812.RData: full_lm_5000_0.95_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_96812.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_96812.R'

full_lm_1000_0.9_59521.RData: full_lm_1000_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_59521.R'

full_mars_500_0.9_13903.RData: full_mars_500_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_13903.R'

full_cart_5000_0.9_87320.RData: full_cart_5000_0.9_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_87320.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_87320.R'

full_mars_1000_0.95_25173.RData: full_mars_1000_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_25173.R'

full_mars_5000_0.9_43307.RData: full_mars_5000_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_43307.R'

full_cart_5000_0.95_45399.RData: full_cart_5000_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_45399.R'

full_mars_5000_0.9_8229.RData: full_mars_5000_0.9_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_8229.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_8229.R'

full_mars_5000_0.95_87320.RData: full_mars_5000_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_87320.R'

full_cart_1000_0.9_72611.RData: full_cart_1000_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_72611.R'

full_cart_1000_0.95_25173.RData: full_cart_1000_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_25173.R'

full_lm_100_0.95_25173.RData: full_lm_100_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_25173.R'

full_lm_1000_0.9_6519.RData: full_lm_1000_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_6519.R'

full_cart_100_0.9_45399.RData: full_cart_100_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_45399.R'

full_mars_1000_0.9_25305.RData: full_mars_1000_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_25305.R'

full_cart_5000_0.9_90597.RData: full_cart_5000_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_90597.R'

full_cart_1000_0.95_11571.RData: full_cart_1000_0.95_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_11571.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_11571.R'

full_cart_500_0.9_25173.RData: full_cart_500_0.9_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_25173.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_25173.R'

full_lm_100_0.95_13903.RData: full_lm_100_0.95_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_13903.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_13903.R'

full_cart_1000_0.95_8229.RData: full_cart_1000_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_8229.R'

full_lm_1000_0.95_45399.RData: full_lm_1000_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_45399.R'

full_cart_1000_0.9_81580.RData: full_cart_1000_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_81580.R'

full_mars_1000_0.9_43809.RData: full_mars_1000_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_43809.R'

full_lm_5000_0.95_32618.RData: full_lm_5000_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_32618.R'

full_cart_5000_0.9_96812.RData: full_cart_5000_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_96812.R'

full_lm_5000_0.95_69586.RData: full_lm_5000_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_69586.R'

full_cart_5000_0.95_32618.RData: full_cart_5000_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_32618.R'

full_mars_500_0.9_90597.RData: full_mars_500_0.9_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_90597.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_90597.R'

full_mars_1000_0.9_81580.RData: full_mars_1000_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_81580.R'

full_mars_1000_0.9_59521.RData: full_mars_1000_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_59521.R'

full_mars_100_0.95_92490.RData: full_mars_100_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_92490.R'

full_mars_500_0.95_24388.RData: full_mars_500_0.95_24388.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_24388.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_24388.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_24388.R'

full_lm_100_0.9_92490.RData: full_lm_100_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_92490.R'

full_mars_5000_0.9_69586.RData: full_mars_5000_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_69586.R'

full_mars_5000_0.9_36244.RData: full_mars_5000_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_36244.R'

full_mars_5000_0.95_92199.RData: full_mars_5000_0.95_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_92199.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_92199.R'

full_lm_1000_0.9_11571.RData: full_lm_1000_0.9_11571.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_11571.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_11571.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_11571.R'

full_mars_500_0.95_43809.RData: full_mars_500_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_43809.R'

full_lm_1000_0.9_45399.RData: full_lm_1000_0.9_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_45399.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_45399.R'

full_cart_1000_0.95_45399.RData: full_cart_1000_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_45399.R'

full_mars_100_0.9_19242.RData: full_mars_100_0.9_19242.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_19242.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_19242.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_19242.R'

full_mars_100_0.95_12204.RData: full_mars_100_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_12204.R'

full_cart_100_0.9_43809.RData: full_cart_100_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_43809.R'

full_cart_1000_0.9_96812.RData: full_cart_1000_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.9_96812.R'

full_lm_500_0.9_92490.RData: full_lm_500_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_92490.R'

full_cart_1000_0.95_87320.RData: full_cart_1000_0.95_87320.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_87320.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_87320.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_87320.R'

full_mars_100_0.9_22306.RData: full_mars_100_0.9_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_22306.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_22306.R'

full_lm_5000_0.95_43809.RData: full_lm_5000_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_43809.R'

full_mars_100_0.95_25173.RData: full_mars_100_0.95_25173.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_25173.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_25173.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_25173.R'

full_cart_100_0.9_92490.RData: full_cart_100_0.9_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_92490.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_92490.R'

full_mars_5000_0.9_43809.RData: full_mars_5000_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_43809.R'

full_mars_100_0.95_36244.RData: full_mars_100_0.95_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_36244.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_36244.R'

full_lm_1000_0.9_72611.RData: full_lm_1000_0.9_72611.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_72611.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_72611.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_72611.R'

full_lm_5000_0.95_81580.RData: full_lm_5000_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_81580.R'

full_mars_500_0.9_12204.RData: full_mars_500_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_12204.R'

full_cart_500_0.9_81580.RData: full_cart_500_0.9_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_500_0.9_81580.R'
	@$(RCMD) BATCH --vanilla full_cart_500_0.9_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_500_0.9_81580.R'

full_mars_5000_0.95_8229.RData: full_mars_5000_0.95_8229.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_8229.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_8229.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_8229.R'

full_cart_5000_0.95_43809.RData: full_cart_5000_0.95_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_43809.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_43809.R'

full_lm_5000_0.95_90597.RData: full_lm_5000_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_90597.R'

full_lm_1000_0.9_92199.RData: full_lm_1000_0.9_92199.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_92199.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_92199.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_92199.R'

full_lm_100_0.95_81580.RData: full_lm_100_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_81580.R'

full_mars_500_0.95_12204.RData: full_mars_500_0.95_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_12204.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_12204.R'

full_mars_1000_0.9_12204.RData: full_mars_1000_0.9_12204.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.9_12204.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.9_12204.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.9_12204.R'

full_cart_100_0.95_45399.RData: full_cart_100_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.95_45399.R'

full_lm_5000_0.95_92490.RData: full_lm_5000_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_92490.R'

full_mars_500_0.95_81580.RData: full_mars_500_0.95_81580.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.95_81580.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.95_81580.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.95_81580.R'

full_lm_500_0.9_36244.RData: full_lm_500_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_36244.R'

full_lm_100_0.95_99526.RData: full_lm_100_0.95_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.95_99526.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.95_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.95_99526.R'

full_lm_500_0.9_43307.RData: full_lm_500_0.9_43307.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_43307.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_43307.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_43307.R'

full_cart_100_0.9_96812.RData: full_cart_100_0.9_96812.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_100_0.9_96812.R'
	@$(RCMD) BATCH --vanilla full_cart_100_0.9_96812.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_100_0.9_96812.R'

full_lm_5000_0.9_6519.RData: full_lm_5000_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.9_6519.R'

full_mars_500_0.9_36244.RData: full_mars_500_0.9_36244.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_500_0.9_36244.R'
	@$(RCMD) BATCH --vanilla full_mars_500_0.9_36244.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_500_0.9_36244.R'

full_cart_1000_0.95_22306.RData: full_cart_1000_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_1000_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_cart_1000_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_1000_0.95_22306.R'

full_lm_500_0.9_43809.RData: full_lm_500_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_43809.R'

full_mars_5000_0.9_25305.RData: full_mars_5000_0.9_25305.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_25305.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_25305.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_25305.R'

full_cart_5000_0.9_6519.RData: full_cart_5000_0.9_6519.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.9_6519.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.9_6519.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.9_6519.R'

full_cart_5000_0.95_69586.RData: full_cart_5000_0.95_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_69586.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_69586.R'

full_cart_5000_0.95_92490.RData: full_cart_5000_0.95_92490.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_cart_5000_0.95_92490.R'
	@$(RCMD) BATCH --vanilla full_cart_5000_0.95_92490.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_cart_5000_0.95_92490.R'

full_mars_100_0.95_22306.RData: full_mars_100_0.95_22306.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.95_22306.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.95_22306.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.95_22306.R'

full_lm_100_0.9_69586.RData: full_lm_100_0.9_69586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_69586.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_69586.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_69586.R'

full_lm_5000_0.95_45399.RData: full_lm_5000_0.95_45399.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_5000_0.95_45399.R'
	@$(RCMD) BATCH --vanilla full_lm_5000_0.95_45399.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_5000_0.95_45399.R'

full_lm_500_0.9_59521.RData: full_lm_500_0.9_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_500_0.9_59521.R'
	@$(RCMD) BATCH --vanilla full_lm_500_0.9_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_500_0.9_59521.R'

full_lm_1000_0.9_43809.RData: full_lm_1000_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.9_43809.R'

full_mars_5000_0.95_59521.RData: full_mars_5000_0.95_59521.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.95_59521.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.95_59521.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.95_59521.R'

full_lm_100_0.9_99526.RData: full_lm_100_0.9_99526.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_100_0.9_99526.R'
	@$(RCMD) BATCH --vanilla full_lm_100_0.9_99526.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_100_0.9_99526.R'

full_mars_5000_0.9_13903.RData: full_mars_5000_0.9_13903.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_5000_0.9_13903.R'
	@$(RCMD) BATCH --vanilla full_mars_5000_0.9_13903.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_5000_0.9_13903.R'

full_mars_1000_0.95_32618.RData: full_mars_1000_0.95_32618.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_1000_0.95_32618.R'
	@$(RCMD) BATCH --vanilla full_mars_1000_0.95_32618.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_1000_0.95_32618.R'

full_lm_1000_0.95_90597.RData: full_lm_1000_0.95_90597.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_lm_1000_0.95_90597.R'
	@$(RCMD) BATCH --vanilla full_lm_1000_0.95_90597.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_lm_1000_0.95_90597.R'

full_mars_100_0.9_43809.RData: full_mars_100_0.9_43809.R 
	@date '+ %Y-%m-%d %H:%M:%S: + full_mars_100_0.9_43809.R'
	@$(RCMD) BATCH --vanilla full_mars_100_0.9_43809.R
	@date '+ %Y-%m-%d %H:%M:%S: - full_mars_100_0.9_43809.R'
