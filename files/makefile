SHELL = /bin/bash
R    ?= R 
RCMD  =@$(R) CMD
TIMESTAMP = $(shell  date '+%Y-%m-%d-%H-%M')
here=${PWD}/..
all: cv_mars_cv_500_0.95_63935.RData cv_mlp_cv_1000_0.95_85818.RData cv_mlp_boot_5000_0.95_85818.RData cv_mars_boot_5000_0.9_90601.RData cv_lm_boot_1000_0.9_88428.RData cv_mars_boot_5000_0.95_54686.RData cv_mlp_boot_1000_0.95_61920.RData cv_lm_cv_5000_0.9_13308.RData cv_lm_boot_5000_0.95_25462.RData cv_mars_boot_1000_0.95_94074.RData cv_mars_boot_5000_0.95_70246.RData cv_lm_boot_500_0.95_90601.RData cv_mars_boot_500_0.9_82586.RData cv_mars_boot_1000_0.9_9503.RData cv_lm_boot_500_0.95_90992.RData cv_lm_boot_1000_0.95_85818.RData cv_lm_cv_500_0.95_94074.RData cv_mars_cv_1000_0.9_88428.RData cv_mlp_cv_1000_0.95_52080.RData cv_mars_boot_1000_0.95_28082.RData cv_mlp_boot_1000_0.95_37035.RData cv_mlp_boot_1000_0.9_82586.RData cv_mlp_boot_500_0.9_18633.RData cv_lm_cv_5000_0.9_54686.RData cv_mlp_cv_500_0.95_90992.RData cv_lm_boot_5000_0.95_33453.RData cv_mlp_boot_1000_0.95_63935.RData cv_mlp_boot_5000_0.9_54686.RData cv_mars_cv_500_0.9_54686.RData cv_mars_cv_500_0.95_74917.RData cv_mars_cv_500_0.9_65769.RData cv_lm_boot_5000_0.9_52080.RData cv_mlp_cv_500_0.9_11319.RData cv_mlp_boot_5000_0.9_52080.RData cv_lm_cv_500_0.95_54686.RData cv_mlp_boot_500_0.9_82586.RData cv_lm_cv_1000_0.9_52080.RData cv_lm_cv_5000_0.95_11319.RData cv_mlp_cv_500_0.9_38345.RData cv_mlp_cv_1000_0.95_13308.RData cv_mars_boot_5000_0.9_11319.RData cv_mars_boot_1000_0.95_88428.RData cv_mars_boot_5000_0.95_13308.RData cv_mars_boot_5000_0.95_63935.RData cv_mlp_boot_500_0.95_33964.RData cv_lm_boot_1000_0.9_69773.RData cv_mars_boot_1000_0.95_11319.RData cv_mlp_cv_5000_0.95_11319.RData cv_lm_boot_5000_0.95_94074.RData cv_lm_boot_5000_0.95_85818.RData cv_mars_cv_500_0.9_93901.RData cv_mlp_cv_1000_0.9_88428.RData cv_mars_boot_5000_0.95_33453.RData cv_mars_cv_1000_0.95_13308.RData cv_lm_cv_5000_0.95_18633.RData cv_mlp_boot_5000_0.95_54686.RData cv_mars_cv_5000_0.95_93901.RData cv_lm_boot_5000_0.9_94074.RData cv_mlp_boot_500_0.9_74917.RData cv_mlp_boot_500_0.95_11319.RData cv_mars_boot_1000_0.9_63935.RData cv_mlp_cv_500_0.95_65769.RData cv_mars_boot_1000_0.95_18633.RData cv_mlp_boot_500_0.95_82586.RData cv_lm_cv_5000_0.95_85818.RData cv_mars_cv_5000_0.9_69773.RData cv_mars_cv_1000_0.95_38345.RData cv_mars_boot_5000_0.9_52080.RData cv_lm_boot_5000_0.95_37035.RData cv_lm_cv_500_0.95_88428.RData cv_lm_cv_5000_0.95_13308.RData cv_mlp_boot_5000_0.9_90992.RData cv_mlp_cv_1000_0.9_93901.RData cv_lm_boot_5000_0.95_88428.RData cv_mars_cv_500_0.95_38345.RData cv_mlp_cv_5000_0.9_90992.RData cv_lm_cv_500_0.95_70246.RData cv_mlp_cv_5000_0.9_28082.RData cv_lm_cv_5000_0.9_70246.RData cv_mars_cv_500_0.9_82586.RData cv_lm_boot_500_0.9_18633.RData cv_lm_boot_1000_0.9_61920.RData cv_mlp_cv_500_0.95_54686.RData cv_lm_boot_500_0.95_82586.RData cv_mlp_cv_5000_0.95_93901.RData cv_mlp_boot_1000_0.9_28082.RData cv_lm_boot_500_0.95_69773.RData cv_lm_cv_1000_0.9_37035.RData cv_mlp_cv_5000_0.9_88428.RData cv_mars_cv_500_0.9_37035.RData cv_mars_cv_500_0.95_65769.RData cv_mars_boot_5000_0.9_88428.RData cv_mlp_boot_1000_0.9_11319.RData cv_mlp_boot_1000_0.9_94074.RData cv_mars_cv_500_0.95_33453.RData cv_mars_cv_1000_0.9_61920.RData cv_lm_cv_1000_0.9_69773.RData cv_lm_boot_5000_0.95_52080.RData cv_lm_boot_500_0.95_25462.RData cv_mars_boot_5000_0.9_18633.RData cv_mlp_boot_5000_0.95_9503.RData cv_lm_cv_500_0.95_11319.RData cv_lm_cv_5000_0.95_38345.RData cv_mlp_boot_500_0.95_37035.RData cv_mlp_cv_1000_0.9_33453.RData cv_mars_boot_1000_0.9_52080.RData cv_mlp_boot_500_0.9_90601.RData cv_mlp_boot_1000_0.95_13308.RData cv_mlp_boot_500_0.95_33453.RData cv_mars_cv_1000_0.95_82586.RData cv_lm_cv_5000_0.9_33964.RData cv_lm_cv_5000_0.95_54686.RData cv_lm_boot_1000_0.9_38345.RData cv_mars_boot_500_0.95_18633.RData cv_mars_cv_5000_0.9_61920.RData cv_mlp_cv_5000_0.9_18633.RData cv_lm_boot_5000_0.95_82586.RData cv_lm_boot_1000_0.95_11319.RData cv_mars_boot_500_0.9_63935.RData cv_mars_boot_1000_0.95_38345.RData cv_mars_boot_500_0.95_94074.RData cv_mlp_cv_500_0.9_74917.RData cv_lm_boot_500_0.9_63935.RData cv_mars_boot_500_0.95_9503.RData cv_mlp_cv_1000_0.9_85818.RData cv_mars_cv_5000_0.95_28082.RData cv_lm_cv_1000_0.9_54686.RData cv_mlp_boot_500_0.9_37035.RData cv_mlp_cv_5000_0.9_82586.RData cv_mlp_boot_500_0.95_93901.RData cv_mars_cv_500_0.95_82586.RData cv_mlp_cv_500_0.9_63935.RData cv_lm_boot_1000_0.95_65769.RData cv_mlp_cv_1000_0.9_94074.RData cv_mlp_cv_500_0.95_70246.RData cv_mlp_cv_500_0.9_88428.RData cv_mars_cv_1000_0.9_69773.RData cv_lm_cv_500_0.95_28082.RData cv_mlp_cv_500_0.95_25462.RData cv_lm_cv_1000_0.9_33964.RData cv_mlp_cv_5000_0.95_37035.RData cv_mars_boot_1000_0.95_93901.RData cv_mlp_cv_5000_0.95_33453.RData cv_mars_cv_500_0.95_69773.RData cv_mars_boot_1000_0.9_38345.RData cv_mlp_cv_500_0.95_88428.RData cv_mars_boot_1000_0.95_63935.RData cv_mars_boot_500_0.9_65769.RData cv_lm_cv_1000_0.95_33964.RData cv_mars_boot_500_0.9_38345.RData cv_mlp_cv_1000_0.95_90992.RData cv_mars_cv_500_0.9_90601.RData cv_mlp_cv_5000_0.95_52080.RData cv_mlp_boot_500_0.9_52080.RData cv_mlp_boot_1000_0.9_18633.RData cv_mars_boot_1000_0.95_69773.RData cv_lm_boot_500_0.95_33964.RData cv_mars_boot_500_0.95_85818.RData cv_mlp_cv_1000_0.9_82586.RData cv_mars_cv_1000_0.9_52080.RData cv_lm_boot_500_0.9_37035.RData cv_mlp_boot_5000_0.95_90601.RData cv_lm_cv_1000_0.95_90601.RData cv_lm_cv_1000_0.9_85818.RData cv_mars_boot_5000_0.95_82586.RData cv_mars_cv_5000_0.9_33964.RData cv_mars_boot_1000_0.95_37035.RData cv_mars_cv_5000_0.95_88428.RData cv_lm_boot_1000_0.95_38345.RData cv_mlp_cv_500_0.95_38345.RData cv_lm_cv_500_0.9_74917.RData cv_mars_boot_500_0.95_90992.RData cv_mlp_boot_500_0.95_28082.RData cv_lm_boot_5000_0.95_54686.RData cv_mlp_boot_500_0.9_33453.RData cv_mars_cv_1000_0.95_85818.RData cv_lm_cv_1000_0.95_28082.RData cv_lm_cv_500_0.95_18633.RData cv_lm_cv_5000_0.95_52080.RData cv_mars_boot_500_0.95_74917.RData cv_mlp_boot_5000_0.9_74917.RData cv_lm_boot_1000_0.95_90992.RData cv_mlp_cv_1000_0.95_38345.RData cv_mlp_boot_1000_0.95_28082.RData cv_mlp_boot_5000_0.9_85818.RData cv_mlp_cv_500_0.9_61920.RData cv_mars_cv_1000_0.95_28082.RData cv_mlp_cv_5000_0.95_65769.RData cv_mars_boot_1000_0.95_70246.RData cv_mars_cv_5000_0.9_90601.RData cv_mlp_cv_5000_0.9_69773.RData cv_mars_cv_500_0.9_33964.RData cv_lm_boot_5000_0.9_61920.RData cv_mlp_cv_500_0.9_13308.RData cv_lm_boot_500_0.95_28082.RData cv_lm_cv_5000_0.95_63935.RData cv_mars_cv_500_0.95_90992.RData cv_lm_cv_500_0.9_52080.RData cv_mlp_cv_1000_0.95_63935.RData cv_lm_cv_5000_0.95_61920.RData cv_lm_boot_1000_0.9_70246.RData cv_mlp_cv_1000_0.95_65769.RData cv_lm_boot_5000_0.95_70246.RData cv_mars_boot_500_0.95_33453.RData cv_mlp_cv_500_0.9_90992.RData cv_lm_boot_1000_0.95_9503.RData cv_lm_boot_1000_0.9_90992.RData cv_lm_cv_1000_0.9_61920.RData cv_mars_boot_1000_0.9_70246.RData cv_mars_boot_1000_0.9_88428.RData cv_lm_cv_5000_0.9_38345.RData cv_mars_boot_500_0.9_37035.RData cv_mars_cv_5000_0.95_13308.RData cv_mars_boot_500_0.9_28082.RData cv_mlp_cv_5000_0.95_61920.RData cv_lm_cv_1000_0.9_65769.RData cv_mars_cv_1000_0.9_65769.RData cv_mlp_cv_5000_0.9_33964.RData cv_lm_cv_500_0.9_82586.RData cv_mlp_cv_500_0.95_11319.RData cv_mlp_cv_1000_0.95_70246.RData cv_mars_cv_1000_0.95_63935.RData cv_lm_cv_500_0.95_37035.RData cv_lm_cv_1000_0.95_82586.RData cv_mlp_cv_1000_0.95_94074.RData cv_lm_boot_500_0.95_37035.RData cv_lm_cv_500_0.9_54686.RData cv_mlp_cv_1000_0.9_13308.RData cv_lm_boot_1000_0.95_13308.RData cv_mlp_cv_500_0.9_37035.RData cv_mars_cv_500_0.9_70246.RData cv_lm_boot_500_0.9_85818.RData cv_mars_cv_1000_0.95_11319.RData cv_lm_cv_1000_0.9_90601.RData cv_lm_cv_500_0.9_90601.RData cv_lm_cv_1000_0.9_11319.RData cv_mars_cv_5000_0.9_33453.RData cv_mlp_cv_500_0.95_37035.RData cv_lm_cv_1000_0.95_9503.RData cv_lm_cv_500_0.95_33453.RData cv_mlp_boot_500_0.9_65769.RData cv_mars_boot_1000_0.9_90601.RData cv_lm_boot_1000_0.9_11319.RData cv_lm_boot_5000_0.95_18633.RData cv_mlp_cv_5000_0.95_13308.RData cv_mars_cv_500_0.95_94074.RData cv_mars_cv_500_0.95_18633.RData cv_lm_cv_5000_0.9_37035.RData cv_mlp_boot_1000_0.9_13308.RData cv_mlp_boot_500_0.9_13308.RData cv_mars_boot_5000_0.95_25462.RData cv_mars_cv_500_0.95_52080.RData cv_mlp_cv_1000_0.9_25462.RData cv_mlp_boot_500_0.9_25462.RData cv_lm_cv_1000_0.95_52080.RData cv_lm_boot_5000_0.9_63935.RData cv_lm_boot_500_0.95_54686.RData cv_mars_boot_500_0.95_82586.RData cv_mlp_cv_500_0.95_69773.RData cv_mars_cv_500_0.95_88428.RData cv_mars_cv_5000_0.95_25462.RData cv_lm_cv_500_0.9_37035.RData cv_mlp_cv_1000_0.9_18633.RData cv_mlp_cv_1000_0.95_37035.RData cv_mlp_boot_1000_0.95_85818.RData cv_mlp_boot_1000_0.9_54686.RData cv_mlp_cv_5000_0.95_74917.RData cv_mars_boot_5000_0.95_69773.RData cv_mlp_boot_500_0.95_25462.RData cv_lm_cv_1000_0.9_94074.RData cv_lm_boot_1000_0.9_94074.RData cv_lm_cv_500_0.9_38345.RData cv_mlp_boot_1000_0.9_63935.RData cv_lm_cv_500_0.95_65769.RData cv_mlp_cv_1000_0.95_88428.RData cv_mlp_cv_500_0.9_9503.RData cv_mlp_cv_1000_0.95_90601.RData cv_lm_boot_1000_0.9_65769.RData cv_mars_cv_5000_0.9_94074.RData cv_mlp_boot_1000_0.95_90601.RData cv_mars_boot_5000_0.95_52080.RData cv_mars_cv_1000_0.9_85818.RData cv_lm_boot_5000_0.9_54686.RData cv_lm_boot_1000_0.9_37035.RData cv_mlp_boot_1000_0.9_65769.RData cv_lm_boot_500_0.9_90601.RData cv_mars_boot_500_0.95_54686.RData cv_mars_boot_5000_0.9_54686.RData cv_mars_boot_1000_0.9_13308.RData cv_lm_cv_500_0.9_69773.RData cv_mars_cv_5000_0.9_88428.RData cv_mlp_boot_1000_0.95_90992.RData cv_mlp_cv_500_0.9_65769.RData cv_mars_boot_500_0.95_69773.RData cv_mlp_cv_1000_0.95_11319.RData cv_mars_boot_500_0.9_25462.RData cv_lm_cv_500_0.95_9503.RData cv_lm_boot_1000_0.9_85818.RData cv_mars_cv_1000_0.9_9503.RData cv_mlp_boot_5000_0.95_94074.RData cv_mars_boot_500_0.95_13308.RData cv_mars_cv_500_0.9_88428.RData cv_mars_cv_1000_0.9_74917.RData cv_mars_boot_1000_0.9_94074.RData cv_mars_cv_1000_0.9_82586.RData cv_mlp_cv_500_0.95_33453.RData cv_lm_cv_5000_0.9_33453.RData cv_mlp_boot_500_0.9_61920.RData cv_lm_cv_500_0.95_85818.RData cv_mlp_boot_5000_0.95_18633.RData cv_mlp_boot_1000_0.95_25462.RData cv_mlp_boot_5000_0.95_33453.RData cv_lm_cv_500_0.9_93901.RData cv_mars_boot_1000_0.95_33964.RData cv_lm_cv_1000_0.95_63935.RData cv_lm_boot_5000_0.9_90992.RData cv_mars_cv_5000_0.9_28082.RData cv_mlp_cv_1000_0.9_33964.RData cv_mars_boot_500_0.9_70246.RData cv_lm_boot_1000_0.95_93901.RData cv_lm_cv_1000_0.9_63935.RData cv_mlp_boot_1000_0.95_69773.RData cv_mlp_boot_1000_0.9_88428.RData cv_mlp_cv_1000_0.95_61920.RData cv_mlp_cv_500_0.9_33964.RData cv_lm_cv_5000_0.95_28082.RData cv_mlp_boot_1000_0.9_85818.RData cv_mars_boot_1000_0.95_65769.RData cv_lm_cv_500_0.9_9503.RData cv_mlp_boot_500_0.95_54686.RData cv_mars_boot_500_0.9_61920.RData cv_mars_boot_500_0.9_69773.RData cv_mars_boot_5000_0.95_9503.RData cv_mlp_cv_500_0.9_93901.RData cv_mlp_cv_5000_0.9_25462.RData cv_lm_cv_1000_0.95_74917.RData cv_lm_cv_5000_0.95_90992.RData cv_lm_cv_1000_0.95_93901.RData cv_lm_boot_500_0.9_90992.RData cv_mars_boot_5000_0.9_93901.RData cv_mars_boot_5000_0.9_69773.RData cv_mars_cv_1000_0.9_18633.RData cv_mlp_boot_5000_0.95_88428.RData cv_lm_cv_5000_0.95_65769.RData cv_mars_cv_1000_0.9_37035.RData cv_mars_cv_5000_0.95_90992.RData cv_mlp_boot_5000_0.95_63935.RData cv_mars_cv_5000_0.95_85818.RData cv_mars_boot_500_0.9_52080.RData cv_mlp_boot_5000_0.95_11319.RData cv_mlp_boot_5000_0.95_52080.RData cv_mlp_boot_5000_0.9_38345.RData cv_mars_cv_1000_0.9_94074.RData cv_lm_cv_5000_0.95_74917.RData cv_lm_boot_5000_0.9_93901.RData cv_mlp_cv_500_0.9_94074.RData cv_lm_boot_1000_0.95_33964.RData cv_mlp_cv_500_0.9_70246.RData cv_mlp_cv_500_0.9_90601.RData cv_mlp_boot_1000_0.95_93901.RData cv_lm_cv_1000_0.95_70246.RData cv_lm_cv_500_0.95_13308.RData cv_mlp_cv_1000_0.9_90601.RData cv_mars_cv_1000_0.95_9503.RData cv_lm_boot_5000_0.9_38345.RData cv_lm_boot_500_0.95_33453.RData cv_mars_boot_500_0.95_11319.RData cv_lm_boot_1000_0.9_82586.RData cv_mars_cv_5000_0.9_25462.RData cv_mlp_cv_500_0.95_93901.RData cv_mlp_cv_500_0.9_82586.RData cv_mlp_boot_500_0.9_93901.RData cv_mlp_boot_5000_0.9_13308.RData cv_mlp_boot_1000_0.9_9503.RData cv_mars_cv_500_0.95_54686.RData cv_mlp_boot_5000_0.9_61920.RData cv_mars_boot_1000_0.9_54686.RData cv_mlp_boot_500_0.9_38345.RData cv_mlp_cv_1000_0.9_70246.RData cv_mars_cv_5000_0.95_69773.RData cv_lm_boot_5000_0.9_13308.RData cv_mlp_cv_1000_0.9_69773.RData cv_lm_boot_500_0.9_28082.RData cv_mlp_cv_5000_0.95_25462.RData cv_mlp_boot_500_0.95_18633.RData cv_mars_cv_1000_0.9_54686.RData cv_mlp_cv_500_0.9_18633.RData cv_mlp_boot_5000_0.9_90601.RData cv_lm_boot_500_0.95_88428.RData cv_lm_boot_5000_0.9_18633.RData cv_mars_cv_5000_0.95_38345.RData cv_mlp_cv_500_0.9_54686.RData cv_lm_boot_5000_0.9_65769.RData cv_mars_cv_1000_0.95_25462.RData cv_mlp_boot_500_0.95_9503.RData cv_lm_cv_1000_0.9_93901.RData cv_lm_cv_1000_0.9_90992.RData cv_lm_boot_1000_0.9_33453.RData cv_mars_cv_500_0.95_61920.RData cv_mars_boot_500_0.9_90992.RData cv_mars_boot_5000_0.95_90992.RData cv_mlp_boot_1000_0.95_88428.RData cv_lm_cv_1000_0.95_54686.RData cv_mars_boot_500_0.95_52080.RData cv_mars_boot_1000_0.9_61920.RData cv_mars_cv_500_0.95_33964.RData cv_lm_boot_500_0.9_11319.RData cv_mlp_boot_5000_0.9_63935.RData cv_mars_cv_500_0.95_85818.RData cv_mars_boot_500_0.95_28082.RData cv_mars_boot_1000_0.95_90992.RData cv_mlp_boot_5000_0.95_69773.RData cv_lm_boot_1000_0.9_25462.RData cv_lm_boot_500_0.9_33964.RData cv_mars_cv_1000_0.95_70246.RData cv_lm_boot_1000_0.9_63935.RData cv_mlp_cv_500_0.95_61920.RData cv_mars_boot_500_0.9_93901.RData cv_mlp_cv_5000_0.95_63935.RData cv_mlp_cv_5000_0.95_90992.RData cv_mars_cv_5000_0.95_74917.RData cv_lm_cv_5000_0.95_9503.RData cv_mars_boot_5000_0.95_88428.RData cv_mars_cv_5000_0.9_11319.RData cv_lm_cv_5000_0.9_25462.RData cv_mars_cv_500_0.9_9503.RData cv_lm_cv_1000_0.9_33453.RData cv_mlp_boot_5000_0.95_65769.RData cv_mars_cv_5000_0.95_54686.RData cv_mars_boot_5000_0.9_94074.RData cv_lm_boot_500_0.9_38345.RData cv_mars_cv_500_0.9_52080.RData cv_mars_cv_1000_0.95_88428.RData cv_lm_cv_1000_0.95_90992.RData cv_lm_cv_500_0.95_52080.RData cv_lm_cv_5000_0.9_85818.RData cv_mars_cv_1000_0.9_25462.RData cv_mlp_boot_1000_0.95_52080.RData cv_mlp_cv_5000_0.9_70246.RData cv_mlp_boot_500_0.9_28082.RData cv_lm_boot_1000_0.95_25462.RData cv_lm_cv_500_0.9_11319.RData cv_mlp_boot_1000_0.9_52080.RData cv_lm_cv_500_0.95_82586.RData cv_mlp_boot_500_0.95_69773.RData cv_lm_boot_500_0.9_25462.RData cv_lm_boot_5000_0.9_11319.RData cv_lm_cv_5000_0.9_90992.RData cv_mlp_cv_5000_0.9_61920.RData cv_lm_boot_1000_0.95_28082.RData cv_lm_boot_1000_0.95_63935.RData cv_lm_cv_5000_0.9_28082.RData cv_lm_cv_1000_0.9_18633.RData cv_mars_boot_1000_0.95_25462.RData cv_lm_cv_1000_0.95_18633.RData cv_mars_boot_5000_0.9_90992.RData cv_mars_cv_1000_0.95_54686.RData cv_mars_cv_5000_0.95_63935.RData cv_mars_cv_5000_0.9_54686.RData cv_mlp_boot_5000_0.9_18633.RData cv_lm_boot_500_0.95_94074.RData cv_mlp_cv_5000_0.95_82586.RData cv_mars_boot_5000_0.9_65769.RData cv_mlp_cv_5000_0.9_37035.RData cv_lm_cv_5000_0.9_63935.RData cv_lm_boot_500_0.95_85818.RData cv_lm_boot_1000_0.95_61920.RData cv_mars_cv_5000_0.95_52080.RData cv_lm_boot_5000_0.95_61920.RData cv_mars_cv_500_0.95_90601.RData cv_mlp_boot_500_0.95_85818.RData cv_mars_boot_1000_0.9_28082.RData cv_mlp_cv_5000_0.9_94074.RData cv_mlp_boot_500_0.9_85818.RData cv_mlp_boot_500_0.95_65769.RData cv_lm_boot_1000_0.9_93901.RData cv_mars_boot_1000_0.9_33453.RData cv_mlp_cv_5000_0.9_13308.RData cv_mlp_boot_1000_0.9_33964.RData cv_mars_boot_1000_0.9_33964.RData cv_lm_boot_500_0.9_52080.RData cv_lm_boot_500_0.95_9503.RData cv_lm_boot_5000_0.95_28082.RData cv_mlp_boot_5000_0.95_25462.RData cv_lm_cv_5000_0.95_90601.RData cv_mlp_cv_500_0.9_25462.RData cv_lm_boot_5000_0.95_90601.RData cv_lm_cv_500_0.9_33453.RData cv_mars_boot_500_0.9_13308.RData cv_mars_boot_5000_0.9_28082.RData cv_lm_boot_5000_0.9_82586.RData cv_mlp_boot_5000_0.9_28082.RData cv_lm_cv_1000_0.95_13308.RData cv_mlp_cv_1000_0.9_74917.RData cv_lm_cv_1000_0.95_33453.RData cv_lm_boot_500_0.95_93901.RData cv_mars_cv_5000_0.95_90601.RData cv_mlp_cv_1000_0.95_69773.RData cv_lm_cv_500_0.95_69773.RData cv_mars_boot_500_0.95_63935.RData cv_lm_boot_5000_0.9_70246.RData cv_lm_cv_500_0.95_61920.RData cv_mars_cv_1000_0.95_74917.RData cv_mlp_boot_1000_0.95_33964.RData cv_lm_boot_500_0.9_70246.RData cv_mars_cv_5000_0.9_90992.RData cv_lm_boot_500_0.95_61920.RData cv_mlp_cv_5000_0.95_69773.RData cv_lm_boot_5000_0.95_11319.RData cv_mars_cv_1000_0.9_93901.RData cv_mars_boot_500_0.95_93901.RData cv_lm_boot_500_0.95_13308.RData cv_mlp_cv_500_0.95_28082.RData cv_mlp_boot_5000_0.95_33964.RData cv_mars_boot_1000_0.9_11319.RData cv_mlp_boot_5000_0.9_33453.RData cv_mlp_boot_5000_0.9_65769.RData cv_mlp_boot_500_0.9_63935.RData cv_lm_cv_500_0.9_85818.RData cv_mars_boot_5000_0.9_33453.RData cv_mlp_cv_1000_0.95_25462.RData cv_mars_boot_1000_0.9_74917.RData cv_mars_cv_5000_0.9_37035.RData cv_mlp_cv_1000_0.95_18633.RData cv_mars_cv_5000_0.9_63935.RData cv_lm_boot_500_0.95_11319.RData cv_mlp_boot_500_0.9_54686.RData cv_mlp_cv_1000_0.9_61920.RData cv_lm_cv_5000_0.95_70246.RData cv_mlp_boot_5000_0.95_13308.RData cv_lm_boot_5000_0.9_88428.RData cv_mars_boot_5000_0.95_85818.RData cv_mars_cv_1000_0.9_28082.RData cv_mars_boot_5000_0.95_65769.RData cv_mlp_boot_5000_0.9_11319.RData cv_mars_boot_500_0.9_74917.RData cv_mlp_cv_500_0.9_52080.RData cv_lm_boot_5000_0.95_33964.RData cv_lm_boot_500_0.95_38345.RData cv_mars_cv_500_0.9_61920.RData cv_lm_cv_500_0.9_70246.RData cv_mlp_cv_500_0.95_52080.RData cv_lm_boot_5000_0.9_90601.RData cv_lm_cv_5000_0.95_33453.RData cv_mlp_cv_1000_0.95_74917.RData cv_mars_boot_500_0.95_38345.RData cv_mars_boot_1000_0.9_82586.RData cv_mars_cv_1000_0.95_33453.RData cv_lm_cv_5000_0.9_52080.RData cv_lm_cv_5000_0.9_94074.RData cv_mlp_boot_500_0.95_38345.RData cv_mlp_boot_5000_0.9_82586.RData cv_mlp_boot_1000_0.95_82586.RData cv_mars_boot_1000_0.9_85818.RData cv_mars_cv_5000_0.95_33964.RData cv_mars_boot_500_0.9_88428.RData cv_mars_boot_5000_0.95_11319.RData cv_mars_cv_5000_0.9_38345.RData cv_mlp_cv_5000_0.9_9503.RData cv_mars_boot_5000_0.95_94074.RData cv_lm_boot_500_0.9_33453.RData cv_lm_boot_500_0.9_82586.RData cv_lm_boot_5000_0.95_9503.RData cv_lm_boot_5000_0.9_37035.RData cv_mars_cv_1000_0.9_90601.RData cv_lm_boot_1000_0.9_74917.RData cv_mlp_boot_5000_0.95_74917.RData cv_mars_boot_1000_0.9_90992.RData cv_mars_cv_500_0.95_70246.RData cv_mlp_boot_500_0.9_69773.RData cv_mlp_boot_500_0.9_90992.RData cv_mlp_boot_500_0.95_70246.RData cv_mars_boot_500_0.9_18633.RData cv_lm_boot_500_0.95_18633.RData cv_lm_cv_1000_0.9_28082.RData cv_mars_boot_5000_0.9_25462.RData cv_mlp_boot_5000_0.9_70246.RData cv_lm_boot_1000_0.95_54686.RData cv_mars_boot_500_0.95_65769.RData cv_lm_boot_500_0.95_74917.RData cv_mars_boot_5000_0.9_33964.RData cv_mlp_cv_500_0.95_74917.RData cv_mlp_boot_1000_0.95_94074.RData cv_lm_boot_1000_0.95_18633.RData cv_mlp_cv_1000_0.9_65769.RData cv_mlp_cv_500_0.9_28082.RData cv_lm_cv_500_0.9_33964.RData cv_mlp_cv_1000_0.95_33453.RData cv_mars_boot_1000_0.9_25462.RData cv_mlp_cv_1000_0.95_54686.RData cv_mlp_cv_500_0.95_9503.RData cv_mars_cv_1000_0.9_70246.RData cv_mars_cv_500_0.95_13308.RData cv_lm_boot_500_0.9_13308.RData cv_lm_cv_5000_0.9_69773.RData cv_lm_cv_500_0.9_90992.RData cv_lm_boot_500_0.9_94074.RData cv_mars_cv_5000_0.95_70246.RData cv_mars_cv_1000_0.95_90601.RData cv_lm_cv_1000_0.9_9503.RData cv_lm_boot_5000_0.9_74917.RData cv_lm_cv_500_0.9_88428.RData cv_mlp_boot_5000_0.95_93901.RData cv_mars_boot_500_0.95_88428.RData cv_mars_boot_5000_0.9_74917.RData cv_mars_boot_500_0.9_54686.RData cv_mars_boot_5000_0.95_28082.RData cv_mlp_boot_5000_0.9_88428.RData cv_mars_boot_1000_0.9_37035.RData cv_lm_boot_1000_0.95_37035.RData cv_mlp_cv_5000_0.9_93901.RData cv_mlp_boot_5000_0.9_37035.RData cv_mars_cv_1000_0.95_93901.RData cv_mars_cv_1000_0.95_90992.RData cv_mars_boot_5000_0.9_85818.RData cv_mlp_boot_1000_0.95_18633.RData cv_mars_cv_1000_0.9_33964.RData cv_mars_boot_1000_0.95_33453.RData cv_mlp_cv_500_0.95_90601.RData cv_mlp_cv_5000_0.95_90601.RData cv_mlp_boot_5000_0.95_38345.RData cv_lm_boot_500_0.9_88428.RData cv_mlp_boot_1000_0.9_38345.RData cv_mars_cv_5000_0.95_9503.RData cv_mars_boot_5000_0.95_93901.RData cv_mlp_cv_1000_0.95_28082.RData cv_lm_boot_500_0.9_54686.RData cv_mars_cv_5000_0.95_33453.RData cv_lm_cv_5000_0.95_33964.RData cv_mars_boot_5000_0.9_70246.RData cv_mars_boot_500_0.9_90601.RData cv_mars_boot_1000_0.95_13308.RData cv_lm_cv_500_0.95_90992.RData cv_lm_boot_5000_0.95_63935.RData cv_mlp_cv_500_0.9_33453.RData cv_mars_cv_5000_0.9_9503.RData cv_mars_boot_1000_0.95_52080.RData cv_mlp_boot_1000_0.9_33453.RData cv_mars_cv_500_0.9_33453.RData cv_mars_cv_500_0.9_63935.RData cv_mlp_boot_500_0.9_33964.RData cv_lm_cv_5000_0.9_90601.RData cv_lm_boot_500_0.95_65769.RData cv_mars_cv_500_0.9_69773.RData cv_mars_boot_1000_0.95_9503.RData cv_mars_boot_5000_0.95_90601.RData cv_lm_cv_500_0.95_38345.RData cv_mars_cv_5000_0.9_13308.RData cv_mlp_cv_5000_0.9_52080.RData cv_mlp_cv_500_0.95_82586.RData cv_mars_cv_500_0.9_18633.RData cv_lm_cv_1000_0.9_38345.RData cv_mars_cv_500_0.9_25462.RData cv_lm_cv_5000_0.9_82586.RData cv_mlp_boot_5000_0.9_9503.RData cv_lm_cv_500_0.95_33964.RData cv_mars_cv_500_0.95_25462.RData cv_mars_cv_5000_0.9_70246.RData cv_mars_boot_5000_0.95_74917.RData cv_mlp_boot_5000_0.95_70246.RData cv_mars_cv_1000_0.9_11319.RData cv_mars_boot_1000_0.95_54686.RData cv_mars_cv_5000_0.9_18633.RData cv_mlp_boot_5000_0.95_61920.RData cv_mars_boot_5000_0.9_63935.RData cv_mlp_boot_5000_0.95_37035.RData cv_mlp_cv_1000_0.95_9503.RData cv_lm_cv_1000_0.95_69773.RData cv_mars_cv_500_0.9_90992.RData cv_mlp_boot_1000_0.9_74917.RData cv_mlp_boot_1000_0.9_90992.RData cv_mlp_boot_500_0.95_52080.RData cv_lm_cv_500_0.9_25462.RData cv_mars_cv_5000_0.95_11319.RData cv_mlp_boot_5000_0.95_28082.RData cv_mars_boot_5000_0.95_38345.RData cv_lm_cv_500_0.9_61920.RData cv_lm_boot_5000_0.9_33964.RData cv_mlp_cv_1000_0.95_93901.RData cv_mars_boot_5000_0.95_61920.RData cv_lm_cv_1000_0.9_70246.RData cv_mars_boot_500_0.9_9503.RData cv_mlp_boot_5000_0.9_25462.RData cv_lm_cv_500_0.95_63935.RData cv_mlp_cv_5000_0.95_94074.RData cv_lm_boot_5000_0.95_38345.RData cv_mlp_boot_1000_0.95_74917.RData cv_mars_boot_1000_0.9_69773.RData cv_lm_cv_5000_0.9_18633.RData cv_mlp_cv_1000_0.95_33964.RData cv_lm_cv_1000_0.9_25462.RData cv_mlp_boot_5000_0.95_90992.RData cv_lm_boot_1000_0.9_54686.RData cv_mlp_cv_5000_0.9_33453.RData cv_mlp_boot_500_0.9_70246.RData cv_mlp_boot_500_0.95_61920.RData cv_mlp_cv_1000_0.9_54686.RData cv_lm_cv_500_0.95_93901.RData cv_mars_boot_1000_0.95_74917.RData cv_mars_cv_5000_0.95_18633.RData cv_mlp_boot_1000_0.9_90601.RData cv_mlp_boot_500_0.95_90992.RData cv_lm_cv_5000_0.9_93901.RData cv_lm_cv_1000_0.95_38345.RData cv_mlp_cv_1000_0.9_28082.RData cv_mars_cv_500_0.95_37035.RData cv_mars_boot_500_0.9_33453.RData cv_mlp_cv_5000_0.95_88428.RData cv_lm_cv_500_0.9_18633.RData cv_mlp_cv_5000_0.95_33964.RData cv_mlp_boot_1000_0.95_9503.RData cv_mlp_boot_1000_0.95_38345.RData cv_mlp_boot_500_0.95_94074.RData cv_mlp_cv_5000_0.95_85818.RData cv_mars_cv_5000_0.9_65769.RData cv_lm_boot_5000_0.9_25462.RData cv_mlp_cv_1000_0.9_63935.RData cv_lm_boot_1000_0.95_74917.RData cv_mlp_boot_1000_0.95_33453.RData cv_mars_boot_500_0.95_70246.RData cv_lm_boot_1000_0.95_90601.RData cv_lm_cv_5000_0.9_65769.RData cv_mars_cv_500_0.95_93901.RData cv_mars_boot_1000_0.95_90601.RData cv_lm_boot_1000_0.9_28082.RData cv_lm_boot_500_0.9_69773.RData cv_lm_cv_5000_0.9_88428.RData cv_lm_boot_5000_0.95_69773.RData cv_lm_cv_500_0.95_25462.RData cv_lm_boot_1000_0.95_69773.RData cv_mars_cv_500_0.95_9503.RData cv_mars_cv_1000_0.9_63935.RData cv_mlp_boot_5000_0.95_82586.RData cv_lm_boot_1000_0.95_82586.RData cv_mlp_boot_5000_0.9_33964.RData cv_mlp_cv_1000_0.9_90992.RData cv_mlp_cv_500_0.95_33964.RData cv_mlp_boot_1000_0.9_37035.RData cv_mars_boot_500_0.95_90601.RData cv_lm_cv_5000_0.95_94074.RData cv_mars_boot_1000_0.9_18633.RData cv_mlp_cv_5000_0.9_90601.RData cv_lm_boot_1000_0.95_88428.RData cv_lm_cv_500_0.9_28082.RData cv_lm_boot_500_0.9_9503.RData cv_mars_cv_1000_0.95_33964.RData cv_lm_cv_1000_0.95_61920.RData cv_mlp_boot_500_0.95_74917.RData cv_mars_boot_1000_0.9_65769.RData cv_mlp_boot_500_0.9_94074.RData cv_lm_boot_1000_0.9_13308.RData cv_lm_cv_5000_0.9_11319.RData cv_mars_cv_1000_0.9_38345.RData cv_mars_boot_5000_0.9_61920.RData cv_mars_cv_1000_0.95_65769.RData cv_mars_boot_5000_0.9_38345.RData cv_mars_cv_500_0.95_11319.RData cv_mlp_cv_500_0.9_85818.RData cv_mlp_boot_500_0.95_13308.RData cv_mars_cv_500_0.9_85818.RData cv_lm_boot_5000_0.95_74917.RData cv_lm_boot_1000_0.9_52080.RData cv_mars_cv_5000_0.9_52080.RData cv_lm_cv_500_0.9_94074.RData cv_mars_cv_500_0.9_28082.RData cv_mlp_boot_5000_0.9_93901.RData cv_mlp_cv_500_0.95_94074.RData cv_mars_cv_5000_0.9_85818.RData cv_lm_cv_1000_0.95_37035.RData cv_mars_boot_500_0.95_25462.RData cv_mars_cv_1000_0.9_13308.RData cv_mlp_boot_1000_0.95_11319.RData cv_lm_cv_5000_0.95_93901.RData cv_lm_boot_1000_0.9_90601.RData cv_mars_cv_5000_0.95_37035.RData cv_mars_cv_5000_0.9_74917.RData cv_lm_cv_5000_0.9_9503.RData cv_mlp_cv_5000_0.9_65769.RData cv_mars_cv_500_0.9_38345.RData cv_mlp_cv_5000_0.95_9503.RData cv_mars_boot_500_0.95_37035.RData cv_mars_boot_500_0.9_33964.RData cv_lm_cv_1000_0.9_88428.RData cv_lm_cv_5000_0.95_37035.RData cv_mars_cv_5000_0.9_93901.RData cv_mlp_boot_1000_0.9_61920.RData cv_lm_boot_5000_0.95_13308.RData cv_mlp_cv_500_0.9_69773.RData cv_mars_cv_5000_0.95_65769.RData cv_mlp_cv_1000_0.9_52080.RData cv_mlp_cv_5000_0.9_74917.RData cv_mars_cv_5000_0.9_82586.RData cv_mars_boot_5000_0.9_37035.RData cv_mlp_boot_5000_0.9_94074.RData cv_mars_boot_5000_0.9_82586.RData cv_mars_cv_500_0.95_28082.RData cv_mars_cv_1000_0.9_90992.RData cv_lm_cv_1000_0.95_94074.RData cv_mlp_cv_5000_0.9_38345.RData cv_mars_boot_5000_0.95_18633.RData cv_mlp_cv_5000_0.95_70246.RData cv_mars_cv_5000_0.95_61920.RData cv_mlp_boot_1000_0.9_70246.RData cv_mlp_cv_1000_0.9_37035.RData cv_mars_cv_500_0.9_74917.RData cv_mars_cv_1000_0.95_52080.RData cv_mars_boot_1000_0.9_93901.RData cv_mars_cv_1000_0.95_61920.RData cv_mlp_cv_1000_0.9_38345.RData cv_lm_cv_5000_0.95_69773.RData cv_mars_boot_1000_0.95_82586.RData cv_mlp_boot_500_0.9_9503.RData cv_mlp_boot_1000_0.95_65769.RData cv_mars_boot_1000_0.95_61920.RData cv_lm_cv_500_0.95_90601.RData cv_lm_cv_500_0.9_63935.RData cv_lm_cv_1000_0.9_74917.RData cv_mlp_boot_1000_0.95_70246.RData cv_lm_boot_500_0.9_65769.RData cv_mlp_boot_1000_0.9_93901.RData cv_lm_cv_500_0.9_65769.RData cv_mlp_boot_500_0.95_63935.RData cv_mars_cv_5000_0.95_82586.RData cv_mars_cv_1000_0.95_94074.RData cv_mlp_boot_1000_0.9_25462.RData cv_mlp_boot_500_0.95_90601.RData cv_mars_cv_500_0.9_11319.RData cv_mlp_cv_1000_0.9_9503.RData cv_mars_boot_5000_0.9_13308.RData cv_lm_cv_500_0.95_74917.RData cv_mlp_cv_5000_0.9_85818.RData cv_mlp_boot_1000_0.95_54686.RData cv_mlp_boot_500_0.9_11319.RData cv_lm_cv_1000_0.9_82586.RData cv_lm_boot_500_0.95_70246.RData cv_lm_cv_1000_0.95_11319.RData cv_mlp_boot_500_0.9_88428.RData cv_mlp_cv_500_0.95_85818.RData cv_lm_boot_500_0.95_63935.RData cv_mlp_cv_5000_0.95_28082.RData cv_lm_boot_5000_0.95_65769.RData cv_mars_boot_500_0.9_85818.RData cv_lm_boot_5000_0.9_9503.RData cv_lm_cv_5000_0.9_61920.RData cv_mlp_cv_5000_0.95_54686.RData cv_mars_cv_1000_0.95_18633.RData cv_lm_boot_1000_0.95_70246.RData cv_lm_cv_5000_0.95_82586.RData cv_mars_cv_500_0.9_94074.RData cv_mars_boot_5000_0.95_37035.RData cv_lm_boot_1000_0.9_18633.RData cv_lm_cv_5000_0.9_74917.RData cv_mars_boot_500_0.9_94074.RData cv_lm_boot_5000_0.95_90992.RData cv_lm_cv_5000_0.95_25462.RData cv_mlp_boot_5000_0.9_69773.RData cv_lm_cv_500_0.9_13308.RData cv_lm_boot_5000_0.9_85818.RData cv_lm_boot_500_0.9_74917.RData cv_mlp_cv_5000_0.9_54686.RData cv_mlp_cv_500_0.95_18633.RData cv_lm_boot_1000_0.95_33453.RData cv_mlp_cv_500_0.95_13308.RData cv_mlp_cv_5000_0.9_11319.RData cv_mars_boot_500_0.9_11319.RData cv_lm_cv_1000_0.95_85818.RData cv_lm_boot_1000_0.9_33964.RData cv_lm_boot_1000_0.95_94074.RData cv_mars_boot_500_0.95_61920.RData cv_lm_boot_500_0.95_52080.RData cv_mars_cv_1000_0.95_37035.RData cv_lm_boot_5000_0.9_69773.RData cv_lm_boot_5000_0.9_33453.RData cv_mars_boot_1000_0.95_85818.RData cv_lm_boot_1000_0.95_52080.RData cv_mlp_cv_5000_0.9_63935.RData cv_mars_boot_5000_0.9_9503.RData cv_mlp_boot_500_0.95_88428.RData cv_mlp_cv_1000_0.95_82586.RData cv_mlp_boot_1000_0.9_69773.RData cv_lm_boot_500_0.9_61920.RData cv_lm_boot_5000_0.95_93901.RData cv_mars_boot_5000_0.95_33964.RData cv_lm_boot_1000_0.9_9503.RData cv_lm_cv_1000_0.95_65769.RData cv_mlp_cv_1000_0.9_11319.RData cv_mlp_cv_5000_0.95_38345.RData cv_lm_boot_500_0.9_93901.RData cv_lm_cv_1000_0.95_25462.RData cv_mlp_cv_500_0.95_63935.RData cv_lm_cv_5000_0.95_88428.RData cv_mars_cv_5000_0.95_94074.RData cv_mars_cv_1000_0.9_33453.RData cv_mars_boot_500_0.95_33964.RData cv_mlp_cv_5000_0.95_18633.RData cv_mars_cv_1000_0.95_69773.RData cv_lm_cv_1000_0.95_88428.RData cv_lm_boot_5000_0.9_28082.RData cv_lm_cv_1000_0.9_13308.RData cv_mars_cv_500_0.9_13308.RData

cv_mars_cv_500_0.95_63935.RData: cv_mars_cv_500_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_63935.R'

cv_mlp_cv_1000_0.95_85818.RData: cv_mlp_cv_1000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_85818.R'

cv_mlp_boot_5000_0.95_85818.RData: cv_mlp_boot_5000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_85818.R'

cv_mars_boot_5000_0.9_90601.RData: cv_mars_boot_5000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_90601.R'

cv_lm_boot_1000_0.9_88428.RData: cv_lm_boot_1000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_88428.R'

cv_mars_boot_5000_0.95_54686.RData: cv_mars_boot_5000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_54686.R'

cv_mlp_boot_1000_0.95_61920.RData: cv_mlp_boot_1000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_61920.R'

cv_lm_cv_5000_0.9_13308.RData: cv_lm_cv_5000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_13308.R'

cv_lm_boot_5000_0.95_25462.RData: cv_lm_boot_5000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_25462.R'

cv_mars_boot_1000_0.95_94074.RData: cv_mars_boot_1000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_94074.R'

cv_mars_boot_5000_0.95_70246.RData: cv_mars_boot_5000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_70246.R'

cv_lm_boot_500_0.95_90601.RData: cv_lm_boot_500_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_90601.R'

cv_mars_boot_500_0.9_82586.RData: cv_mars_boot_500_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_82586.R'

cv_mars_boot_1000_0.9_9503.RData: cv_mars_boot_1000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_9503.R'

cv_lm_boot_500_0.95_90992.RData: cv_lm_boot_500_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_90992.R'

cv_lm_boot_1000_0.95_85818.RData: cv_lm_boot_1000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_85818.R'

cv_lm_cv_500_0.95_94074.RData: cv_lm_cv_500_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_94074.R'

cv_mars_cv_1000_0.9_88428.RData: cv_mars_cv_1000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_88428.R'

cv_mlp_cv_1000_0.95_52080.RData: cv_mlp_cv_1000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_52080.R'

cv_mars_boot_1000_0.95_28082.RData: cv_mars_boot_1000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_28082.R'

cv_mlp_boot_1000_0.95_37035.RData: cv_mlp_boot_1000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_37035.R'

cv_mlp_boot_1000_0.9_82586.RData: cv_mlp_boot_1000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_82586.R'

cv_mlp_boot_500_0.9_18633.RData: cv_mlp_boot_500_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_18633.R'

cv_lm_cv_5000_0.9_54686.RData: cv_lm_cv_5000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_54686.R'

cv_mlp_cv_500_0.95_90992.RData: cv_mlp_cv_500_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_90992.R'

cv_lm_boot_5000_0.95_33453.RData: cv_lm_boot_5000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_33453.R'

cv_mlp_boot_1000_0.95_63935.RData: cv_mlp_boot_1000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_63935.R'

cv_mlp_boot_5000_0.9_54686.RData: cv_mlp_boot_5000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_54686.R'

cv_mars_cv_500_0.9_54686.RData: cv_mars_cv_500_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_54686.R'

cv_mars_cv_500_0.95_74917.RData: cv_mars_cv_500_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_74917.R'

cv_mars_cv_500_0.9_65769.RData: cv_mars_cv_500_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_65769.R'

cv_lm_boot_5000_0.9_52080.RData: cv_lm_boot_5000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_52080.R'

cv_mlp_cv_500_0.9_11319.RData: cv_mlp_cv_500_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_11319.R'

cv_mlp_boot_5000_0.9_52080.RData: cv_mlp_boot_5000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_52080.R'

cv_lm_cv_500_0.95_54686.RData: cv_lm_cv_500_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_54686.R'

cv_mlp_boot_500_0.9_82586.RData: cv_mlp_boot_500_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_82586.R'

cv_lm_cv_1000_0.9_52080.RData: cv_lm_cv_1000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_52080.R'

cv_lm_cv_5000_0.95_11319.RData: cv_lm_cv_5000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_11319.R'

cv_mlp_cv_500_0.9_38345.RData: cv_mlp_cv_500_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_38345.R'

cv_mlp_cv_1000_0.95_13308.RData: cv_mlp_cv_1000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_13308.R'

cv_mars_boot_5000_0.9_11319.RData: cv_mars_boot_5000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_11319.R'

cv_mars_boot_1000_0.95_88428.RData: cv_mars_boot_1000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_88428.R'

cv_mars_boot_5000_0.95_13308.RData: cv_mars_boot_5000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_13308.R'

cv_mars_boot_5000_0.95_63935.RData: cv_mars_boot_5000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_63935.R'

cv_mlp_boot_500_0.95_33964.RData: cv_mlp_boot_500_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_33964.R'

cv_lm_boot_1000_0.9_69773.RData: cv_lm_boot_1000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_69773.R'

cv_mars_boot_1000_0.95_11319.RData: cv_mars_boot_1000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_11319.R'

cv_mlp_cv_5000_0.95_11319.RData: cv_mlp_cv_5000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_11319.R'

cv_lm_boot_5000_0.95_94074.RData: cv_lm_boot_5000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_94074.R'

cv_lm_boot_5000_0.95_85818.RData: cv_lm_boot_5000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_85818.R'

cv_mars_cv_500_0.9_93901.RData: cv_mars_cv_500_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_93901.R'

cv_mlp_cv_1000_0.9_88428.RData: cv_mlp_cv_1000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_88428.R'

cv_mars_boot_5000_0.95_33453.RData: cv_mars_boot_5000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_33453.R'

cv_mars_cv_1000_0.95_13308.RData: cv_mars_cv_1000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_13308.R'

cv_lm_cv_5000_0.95_18633.RData: cv_lm_cv_5000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_18633.R'

cv_mlp_boot_5000_0.95_54686.RData: cv_mlp_boot_5000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_54686.R'

cv_mars_cv_5000_0.95_93901.RData: cv_mars_cv_5000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_93901.R'

cv_lm_boot_5000_0.9_94074.RData: cv_lm_boot_5000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_94074.R'

cv_mlp_boot_500_0.9_74917.RData: cv_mlp_boot_500_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_74917.R'

cv_mlp_boot_500_0.95_11319.RData: cv_mlp_boot_500_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_11319.R'

cv_mars_boot_1000_0.9_63935.RData: cv_mars_boot_1000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_63935.R'

cv_mlp_cv_500_0.95_65769.RData: cv_mlp_cv_500_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_65769.R'

cv_mars_boot_1000_0.95_18633.RData: cv_mars_boot_1000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_18633.R'

cv_mlp_boot_500_0.95_82586.RData: cv_mlp_boot_500_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_82586.R'

cv_lm_cv_5000_0.95_85818.RData: cv_lm_cv_5000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_85818.R'

cv_mars_cv_5000_0.9_69773.RData: cv_mars_cv_5000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_69773.R'

cv_mars_cv_1000_0.95_38345.RData: cv_mars_cv_1000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_38345.R'

cv_mars_boot_5000_0.9_52080.RData: cv_mars_boot_5000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_52080.R'

cv_lm_boot_5000_0.95_37035.RData: cv_lm_boot_5000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_37035.R'

cv_lm_cv_500_0.95_88428.RData: cv_lm_cv_500_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_88428.R'

cv_lm_cv_5000_0.95_13308.RData: cv_lm_cv_5000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_13308.R'

cv_mlp_boot_5000_0.9_90992.RData: cv_mlp_boot_5000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_90992.R'

cv_mlp_cv_1000_0.9_93901.RData: cv_mlp_cv_1000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_93901.R'

cv_lm_boot_5000_0.95_88428.RData: cv_lm_boot_5000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_88428.R'

cv_mars_cv_500_0.95_38345.RData: cv_mars_cv_500_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_38345.R'

cv_mlp_cv_5000_0.9_90992.RData: cv_mlp_cv_5000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_90992.R'

cv_lm_cv_500_0.95_70246.RData: cv_lm_cv_500_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_70246.R'

cv_mlp_cv_5000_0.9_28082.RData: cv_mlp_cv_5000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_28082.R'

cv_lm_cv_5000_0.9_70246.RData: cv_lm_cv_5000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_70246.R'

cv_mars_cv_500_0.9_82586.RData: cv_mars_cv_500_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_82586.R'

cv_lm_boot_500_0.9_18633.RData: cv_lm_boot_500_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_18633.R'

cv_lm_boot_1000_0.9_61920.RData: cv_lm_boot_1000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_61920.R'

cv_mlp_cv_500_0.95_54686.RData: cv_mlp_cv_500_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_54686.R'

cv_lm_boot_500_0.95_82586.RData: cv_lm_boot_500_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_82586.R'

cv_mlp_cv_5000_0.95_93901.RData: cv_mlp_cv_5000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_93901.R'

cv_mlp_boot_1000_0.9_28082.RData: cv_mlp_boot_1000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_28082.R'

cv_lm_boot_500_0.95_69773.RData: cv_lm_boot_500_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_69773.R'

cv_lm_cv_1000_0.9_37035.RData: cv_lm_cv_1000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_37035.R'

cv_mlp_cv_5000_0.9_88428.RData: cv_mlp_cv_5000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_88428.R'

cv_mars_cv_500_0.9_37035.RData: cv_mars_cv_500_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_37035.R'

cv_mars_cv_500_0.95_65769.RData: cv_mars_cv_500_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_65769.R'

cv_mars_boot_5000_0.9_88428.RData: cv_mars_boot_5000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_88428.R'

cv_mlp_boot_1000_0.9_11319.RData: cv_mlp_boot_1000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_11319.R'

cv_mlp_boot_1000_0.9_94074.RData: cv_mlp_boot_1000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_94074.R'

cv_mars_cv_500_0.95_33453.RData: cv_mars_cv_500_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_33453.R'

cv_mars_cv_1000_0.9_61920.RData: cv_mars_cv_1000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_61920.R'

cv_lm_cv_1000_0.9_69773.RData: cv_lm_cv_1000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_69773.R'

cv_lm_boot_5000_0.95_52080.RData: cv_lm_boot_5000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_52080.R'

cv_lm_boot_500_0.95_25462.RData: cv_lm_boot_500_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_25462.R'

cv_mars_boot_5000_0.9_18633.RData: cv_mars_boot_5000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_18633.R'

cv_mlp_boot_5000_0.95_9503.RData: cv_mlp_boot_5000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_9503.R'

cv_lm_cv_500_0.95_11319.RData: cv_lm_cv_500_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_11319.R'

cv_lm_cv_5000_0.95_38345.RData: cv_lm_cv_5000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_38345.R'

cv_mlp_boot_500_0.95_37035.RData: cv_mlp_boot_500_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_37035.R'

cv_mlp_cv_1000_0.9_33453.RData: cv_mlp_cv_1000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_33453.R'

cv_mars_boot_1000_0.9_52080.RData: cv_mars_boot_1000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_52080.R'

cv_mlp_boot_500_0.9_90601.RData: cv_mlp_boot_500_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_90601.R'

cv_mlp_boot_1000_0.95_13308.RData: cv_mlp_boot_1000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_13308.R'

cv_mlp_boot_500_0.95_33453.RData: cv_mlp_boot_500_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_33453.R'

cv_mars_cv_1000_0.95_82586.RData: cv_mars_cv_1000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_82586.R'

cv_lm_cv_5000_0.9_33964.RData: cv_lm_cv_5000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_33964.R'

cv_lm_cv_5000_0.95_54686.RData: cv_lm_cv_5000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_54686.R'

cv_lm_boot_1000_0.9_38345.RData: cv_lm_boot_1000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_38345.R'

cv_mars_boot_500_0.95_18633.RData: cv_mars_boot_500_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_18633.R'

cv_mars_cv_5000_0.9_61920.RData: cv_mars_cv_5000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_61920.R'

cv_mlp_cv_5000_0.9_18633.RData: cv_mlp_cv_5000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_18633.R'

cv_lm_boot_5000_0.95_82586.RData: cv_lm_boot_5000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_82586.R'

cv_lm_boot_1000_0.95_11319.RData: cv_lm_boot_1000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_11319.R'

cv_mars_boot_500_0.9_63935.RData: cv_mars_boot_500_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_63935.R'

cv_mars_boot_1000_0.95_38345.RData: cv_mars_boot_1000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_38345.R'

cv_mars_boot_500_0.95_94074.RData: cv_mars_boot_500_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_94074.R'

cv_mlp_cv_500_0.9_74917.RData: cv_mlp_cv_500_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_74917.R'

cv_lm_boot_500_0.9_63935.RData: cv_lm_boot_500_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_63935.R'

cv_mars_boot_500_0.95_9503.RData: cv_mars_boot_500_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_9503.R'

cv_mlp_cv_1000_0.9_85818.RData: cv_mlp_cv_1000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_85818.R'

cv_mars_cv_5000_0.95_28082.RData: cv_mars_cv_5000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_28082.R'

cv_lm_cv_1000_0.9_54686.RData: cv_lm_cv_1000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_54686.R'

cv_mlp_boot_500_0.9_37035.RData: cv_mlp_boot_500_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_37035.R'

cv_mlp_cv_5000_0.9_82586.RData: cv_mlp_cv_5000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_82586.R'

cv_mlp_boot_500_0.95_93901.RData: cv_mlp_boot_500_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_93901.R'

cv_mars_cv_500_0.95_82586.RData: cv_mars_cv_500_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_82586.R'

cv_mlp_cv_500_0.9_63935.RData: cv_mlp_cv_500_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_63935.R'

cv_lm_boot_1000_0.95_65769.RData: cv_lm_boot_1000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_65769.R'

cv_mlp_cv_1000_0.9_94074.RData: cv_mlp_cv_1000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_94074.R'

cv_mlp_cv_500_0.95_70246.RData: cv_mlp_cv_500_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_70246.R'

cv_mlp_cv_500_0.9_88428.RData: cv_mlp_cv_500_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_88428.R'

cv_mars_cv_1000_0.9_69773.RData: cv_mars_cv_1000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_69773.R'

cv_lm_cv_500_0.95_28082.RData: cv_lm_cv_500_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_28082.R'

cv_mlp_cv_500_0.95_25462.RData: cv_mlp_cv_500_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_25462.R'

cv_lm_cv_1000_0.9_33964.RData: cv_lm_cv_1000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_33964.R'

cv_mlp_cv_5000_0.95_37035.RData: cv_mlp_cv_5000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_37035.R'

cv_mars_boot_1000_0.95_93901.RData: cv_mars_boot_1000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_93901.R'

cv_mlp_cv_5000_0.95_33453.RData: cv_mlp_cv_5000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_33453.R'

cv_mars_cv_500_0.95_69773.RData: cv_mars_cv_500_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_69773.R'

cv_mars_boot_1000_0.9_38345.RData: cv_mars_boot_1000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_38345.R'

cv_mlp_cv_500_0.95_88428.RData: cv_mlp_cv_500_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_88428.R'

cv_mars_boot_1000_0.95_63935.RData: cv_mars_boot_1000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_63935.R'

cv_mars_boot_500_0.9_65769.RData: cv_mars_boot_500_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_65769.R'

cv_lm_cv_1000_0.95_33964.RData: cv_lm_cv_1000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_33964.R'

cv_mars_boot_500_0.9_38345.RData: cv_mars_boot_500_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_38345.R'

cv_mlp_cv_1000_0.95_90992.RData: cv_mlp_cv_1000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_90992.R'

cv_mars_cv_500_0.9_90601.RData: cv_mars_cv_500_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_90601.R'

cv_mlp_cv_5000_0.95_52080.RData: cv_mlp_cv_5000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_52080.R'

cv_mlp_boot_500_0.9_52080.RData: cv_mlp_boot_500_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_52080.R'

cv_mlp_boot_1000_0.9_18633.RData: cv_mlp_boot_1000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_18633.R'

cv_mars_boot_1000_0.95_69773.RData: cv_mars_boot_1000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_69773.R'

cv_lm_boot_500_0.95_33964.RData: cv_lm_boot_500_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_33964.R'

cv_mars_boot_500_0.95_85818.RData: cv_mars_boot_500_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_85818.R'

cv_mlp_cv_1000_0.9_82586.RData: cv_mlp_cv_1000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_82586.R'

cv_mars_cv_1000_0.9_52080.RData: cv_mars_cv_1000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_52080.R'

cv_lm_boot_500_0.9_37035.RData: cv_lm_boot_500_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_37035.R'

cv_mlp_boot_5000_0.95_90601.RData: cv_mlp_boot_5000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_90601.R'

cv_lm_cv_1000_0.95_90601.RData: cv_lm_cv_1000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_90601.R'

cv_lm_cv_1000_0.9_85818.RData: cv_lm_cv_1000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_85818.R'

cv_mars_boot_5000_0.95_82586.RData: cv_mars_boot_5000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_82586.R'

cv_mars_cv_5000_0.9_33964.RData: cv_mars_cv_5000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_33964.R'

cv_mars_boot_1000_0.95_37035.RData: cv_mars_boot_1000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_37035.R'

cv_mars_cv_5000_0.95_88428.RData: cv_mars_cv_5000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_88428.R'

cv_lm_boot_1000_0.95_38345.RData: cv_lm_boot_1000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_38345.R'

cv_mlp_cv_500_0.95_38345.RData: cv_mlp_cv_500_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_38345.R'

cv_lm_cv_500_0.9_74917.RData: cv_lm_cv_500_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_74917.R'

cv_mars_boot_500_0.95_90992.RData: cv_mars_boot_500_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_90992.R'

cv_mlp_boot_500_0.95_28082.RData: cv_mlp_boot_500_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_28082.R'

cv_lm_boot_5000_0.95_54686.RData: cv_lm_boot_5000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_54686.R'

cv_mlp_boot_500_0.9_33453.RData: cv_mlp_boot_500_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_33453.R'

cv_mars_cv_1000_0.95_85818.RData: cv_mars_cv_1000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_85818.R'

cv_lm_cv_1000_0.95_28082.RData: cv_lm_cv_1000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_28082.R'

cv_lm_cv_500_0.95_18633.RData: cv_lm_cv_500_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_18633.R'

cv_lm_cv_5000_0.95_52080.RData: cv_lm_cv_5000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_52080.R'

cv_mars_boot_500_0.95_74917.RData: cv_mars_boot_500_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_74917.R'

cv_mlp_boot_5000_0.9_74917.RData: cv_mlp_boot_5000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_74917.R'

cv_lm_boot_1000_0.95_90992.RData: cv_lm_boot_1000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_90992.R'

cv_mlp_cv_1000_0.95_38345.RData: cv_mlp_cv_1000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_38345.R'

cv_mlp_boot_1000_0.95_28082.RData: cv_mlp_boot_1000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_28082.R'

cv_mlp_boot_5000_0.9_85818.RData: cv_mlp_boot_5000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_85818.R'

cv_mlp_cv_500_0.9_61920.RData: cv_mlp_cv_500_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_61920.R'

cv_mars_cv_1000_0.95_28082.RData: cv_mars_cv_1000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_28082.R'

cv_mlp_cv_5000_0.95_65769.RData: cv_mlp_cv_5000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_65769.R'

cv_mars_boot_1000_0.95_70246.RData: cv_mars_boot_1000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_70246.R'

cv_mars_cv_5000_0.9_90601.RData: cv_mars_cv_5000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_90601.R'

cv_mlp_cv_5000_0.9_69773.RData: cv_mlp_cv_5000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_69773.R'

cv_mars_cv_500_0.9_33964.RData: cv_mars_cv_500_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_33964.R'

cv_lm_boot_5000_0.9_61920.RData: cv_lm_boot_5000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_61920.R'

cv_mlp_cv_500_0.9_13308.RData: cv_mlp_cv_500_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_13308.R'

cv_lm_boot_500_0.95_28082.RData: cv_lm_boot_500_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_28082.R'

cv_lm_cv_5000_0.95_63935.RData: cv_lm_cv_5000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_63935.R'

cv_mars_cv_500_0.95_90992.RData: cv_mars_cv_500_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_90992.R'

cv_lm_cv_500_0.9_52080.RData: cv_lm_cv_500_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_52080.R'

cv_mlp_cv_1000_0.95_63935.RData: cv_mlp_cv_1000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_63935.R'

cv_lm_cv_5000_0.95_61920.RData: cv_lm_cv_5000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_61920.R'

cv_lm_boot_1000_0.9_70246.RData: cv_lm_boot_1000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_70246.R'

cv_mlp_cv_1000_0.95_65769.RData: cv_mlp_cv_1000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_65769.R'

cv_lm_boot_5000_0.95_70246.RData: cv_lm_boot_5000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_70246.R'

cv_mars_boot_500_0.95_33453.RData: cv_mars_boot_500_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_33453.R'

cv_mlp_cv_500_0.9_90992.RData: cv_mlp_cv_500_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_90992.R'

cv_lm_boot_1000_0.95_9503.RData: cv_lm_boot_1000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_9503.R'

cv_lm_boot_1000_0.9_90992.RData: cv_lm_boot_1000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_90992.R'

cv_lm_cv_1000_0.9_61920.RData: cv_lm_cv_1000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_61920.R'

cv_mars_boot_1000_0.9_70246.RData: cv_mars_boot_1000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_70246.R'

cv_mars_boot_1000_0.9_88428.RData: cv_mars_boot_1000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_88428.R'

cv_lm_cv_5000_0.9_38345.RData: cv_lm_cv_5000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_38345.R'

cv_mars_boot_500_0.9_37035.RData: cv_mars_boot_500_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_37035.R'

cv_mars_cv_5000_0.95_13308.RData: cv_mars_cv_5000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_13308.R'

cv_mars_boot_500_0.9_28082.RData: cv_mars_boot_500_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_28082.R'

cv_mlp_cv_5000_0.95_61920.RData: cv_mlp_cv_5000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_61920.R'

cv_lm_cv_1000_0.9_65769.RData: cv_lm_cv_1000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_65769.R'

cv_mars_cv_1000_0.9_65769.RData: cv_mars_cv_1000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_65769.R'

cv_mlp_cv_5000_0.9_33964.RData: cv_mlp_cv_5000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_33964.R'

cv_lm_cv_500_0.9_82586.RData: cv_lm_cv_500_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_82586.R'

cv_mlp_cv_500_0.95_11319.RData: cv_mlp_cv_500_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_11319.R'

cv_mlp_cv_1000_0.95_70246.RData: cv_mlp_cv_1000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_70246.R'

cv_mars_cv_1000_0.95_63935.RData: cv_mars_cv_1000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_63935.R'

cv_lm_cv_500_0.95_37035.RData: cv_lm_cv_500_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_37035.R'

cv_lm_cv_1000_0.95_82586.RData: cv_lm_cv_1000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_82586.R'

cv_mlp_cv_1000_0.95_94074.RData: cv_mlp_cv_1000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_94074.R'

cv_lm_boot_500_0.95_37035.RData: cv_lm_boot_500_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_37035.R'

cv_lm_cv_500_0.9_54686.RData: cv_lm_cv_500_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_54686.R'

cv_mlp_cv_1000_0.9_13308.RData: cv_mlp_cv_1000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_13308.R'

cv_lm_boot_1000_0.95_13308.RData: cv_lm_boot_1000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_13308.R'

cv_mlp_cv_500_0.9_37035.RData: cv_mlp_cv_500_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_37035.R'

cv_mars_cv_500_0.9_70246.RData: cv_mars_cv_500_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_70246.R'

cv_lm_boot_500_0.9_85818.RData: cv_lm_boot_500_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_85818.R'

cv_mars_cv_1000_0.95_11319.RData: cv_mars_cv_1000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_11319.R'

cv_lm_cv_1000_0.9_90601.RData: cv_lm_cv_1000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_90601.R'

cv_lm_cv_500_0.9_90601.RData: cv_lm_cv_500_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_90601.R'

cv_lm_cv_1000_0.9_11319.RData: cv_lm_cv_1000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_11319.R'

cv_mars_cv_5000_0.9_33453.RData: cv_mars_cv_5000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_33453.R'

cv_mlp_cv_500_0.95_37035.RData: cv_mlp_cv_500_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_37035.R'

cv_lm_cv_1000_0.95_9503.RData: cv_lm_cv_1000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_9503.R'

cv_lm_cv_500_0.95_33453.RData: cv_lm_cv_500_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_33453.R'

cv_mlp_boot_500_0.9_65769.RData: cv_mlp_boot_500_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_65769.R'

cv_mars_boot_1000_0.9_90601.RData: cv_mars_boot_1000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_90601.R'

cv_lm_boot_1000_0.9_11319.RData: cv_lm_boot_1000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_11319.R'

cv_lm_boot_5000_0.95_18633.RData: cv_lm_boot_5000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_18633.R'

cv_mlp_cv_5000_0.95_13308.RData: cv_mlp_cv_5000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_13308.R'

cv_mars_cv_500_0.95_94074.RData: cv_mars_cv_500_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_94074.R'

cv_mars_cv_500_0.95_18633.RData: cv_mars_cv_500_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_18633.R'

cv_lm_cv_5000_0.9_37035.RData: cv_lm_cv_5000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_37035.R'

cv_mlp_boot_1000_0.9_13308.RData: cv_mlp_boot_1000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_13308.R'

cv_mlp_boot_500_0.9_13308.RData: cv_mlp_boot_500_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_13308.R'

cv_mars_boot_5000_0.95_25462.RData: cv_mars_boot_5000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_25462.R'

cv_mars_cv_500_0.95_52080.RData: cv_mars_cv_500_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_52080.R'

cv_mlp_cv_1000_0.9_25462.RData: cv_mlp_cv_1000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_25462.R'

cv_mlp_boot_500_0.9_25462.RData: cv_mlp_boot_500_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_25462.R'

cv_lm_cv_1000_0.95_52080.RData: cv_lm_cv_1000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_52080.R'

cv_lm_boot_5000_0.9_63935.RData: cv_lm_boot_5000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_63935.R'

cv_lm_boot_500_0.95_54686.RData: cv_lm_boot_500_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_54686.R'

cv_mars_boot_500_0.95_82586.RData: cv_mars_boot_500_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_82586.R'

cv_mlp_cv_500_0.95_69773.RData: cv_mlp_cv_500_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_69773.R'

cv_mars_cv_500_0.95_88428.RData: cv_mars_cv_500_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_88428.R'

cv_mars_cv_5000_0.95_25462.RData: cv_mars_cv_5000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_25462.R'

cv_lm_cv_500_0.9_37035.RData: cv_lm_cv_500_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_37035.R'

cv_mlp_cv_1000_0.9_18633.RData: cv_mlp_cv_1000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_18633.R'

cv_mlp_cv_1000_0.95_37035.RData: cv_mlp_cv_1000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_37035.R'

cv_mlp_boot_1000_0.95_85818.RData: cv_mlp_boot_1000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_85818.R'

cv_mlp_boot_1000_0.9_54686.RData: cv_mlp_boot_1000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_54686.R'

cv_mlp_cv_5000_0.95_74917.RData: cv_mlp_cv_5000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_74917.R'

cv_mars_boot_5000_0.95_69773.RData: cv_mars_boot_5000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_69773.R'

cv_mlp_boot_500_0.95_25462.RData: cv_mlp_boot_500_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_25462.R'

cv_lm_cv_1000_0.9_94074.RData: cv_lm_cv_1000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_94074.R'

cv_lm_boot_1000_0.9_94074.RData: cv_lm_boot_1000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_94074.R'

cv_lm_cv_500_0.9_38345.RData: cv_lm_cv_500_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_38345.R'

cv_mlp_boot_1000_0.9_63935.RData: cv_mlp_boot_1000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_63935.R'

cv_lm_cv_500_0.95_65769.RData: cv_lm_cv_500_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_65769.R'

cv_mlp_cv_1000_0.95_88428.RData: cv_mlp_cv_1000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_88428.R'

cv_mlp_cv_500_0.9_9503.RData: cv_mlp_cv_500_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_9503.R'

cv_mlp_cv_1000_0.95_90601.RData: cv_mlp_cv_1000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_90601.R'

cv_lm_boot_1000_0.9_65769.RData: cv_lm_boot_1000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_65769.R'

cv_mars_cv_5000_0.9_94074.RData: cv_mars_cv_5000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_94074.R'

cv_mlp_boot_1000_0.95_90601.RData: cv_mlp_boot_1000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_90601.R'

cv_mars_boot_5000_0.95_52080.RData: cv_mars_boot_5000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_52080.R'

cv_mars_cv_1000_0.9_85818.RData: cv_mars_cv_1000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_85818.R'

cv_lm_boot_5000_0.9_54686.RData: cv_lm_boot_5000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_54686.R'

cv_lm_boot_1000_0.9_37035.RData: cv_lm_boot_1000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_37035.R'

cv_mlp_boot_1000_0.9_65769.RData: cv_mlp_boot_1000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_65769.R'

cv_lm_boot_500_0.9_90601.RData: cv_lm_boot_500_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_90601.R'

cv_mars_boot_500_0.95_54686.RData: cv_mars_boot_500_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_54686.R'

cv_mars_boot_5000_0.9_54686.RData: cv_mars_boot_5000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_54686.R'

cv_mars_boot_1000_0.9_13308.RData: cv_mars_boot_1000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_13308.R'

cv_lm_cv_500_0.9_69773.RData: cv_lm_cv_500_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_69773.R'

cv_mars_cv_5000_0.9_88428.RData: cv_mars_cv_5000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_88428.R'

cv_mlp_boot_1000_0.95_90992.RData: cv_mlp_boot_1000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_90992.R'

cv_mlp_cv_500_0.9_65769.RData: cv_mlp_cv_500_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_65769.R'

cv_mars_boot_500_0.95_69773.RData: cv_mars_boot_500_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_69773.R'

cv_mlp_cv_1000_0.95_11319.RData: cv_mlp_cv_1000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_11319.R'

cv_mars_boot_500_0.9_25462.RData: cv_mars_boot_500_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_25462.R'

cv_lm_cv_500_0.95_9503.RData: cv_lm_cv_500_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_9503.R'

cv_lm_boot_1000_0.9_85818.RData: cv_lm_boot_1000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_85818.R'

cv_mars_cv_1000_0.9_9503.RData: cv_mars_cv_1000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_9503.R'

cv_mlp_boot_5000_0.95_94074.RData: cv_mlp_boot_5000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_94074.R'

cv_mars_boot_500_0.95_13308.RData: cv_mars_boot_500_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_13308.R'

cv_mars_cv_500_0.9_88428.RData: cv_mars_cv_500_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_88428.R'

cv_mars_cv_1000_0.9_74917.RData: cv_mars_cv_1000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_74917.R'

cv_mars_boot_1000_0.9_94074.RData: cv_mars_boot_1000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_94074.R'

cv_mars_cv_1000_0.9_82586.RData: cv_mars_cv_1000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_82586.R'

cv_mlp_cv_500_0.95_33453.RData: cv_mlp_cv_500_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_33453.R'

cv_lm_cv_5000_0.9_33453.RData: cv_lm_cv_5000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_33453.R'

cv_mlp_boot_500_0.9_61920.RData: cv_mlp_boot_500_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_61920.R'

cv_lm_cv_500_0.95_85818.RData: cv_lm_cv_500_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_85818.R'

cv_mlp_boot_5000_0.95_18633.RData: cv_mlp_boot_5000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_18633.R'

cv_mlp_boot_1000_0.95_25462.RData: cv_mlp_boot_1000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_25462.R'

cv_mlp_boot_5000_0.95_33453.RData: cv_mlp_boot_5000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_33453.R'

cv_lm_cv_500_0.9_93901.RData: cv_lm_cv_500_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_93901.R'

cv_mars_boot_1000_0.95_33964.RData: cv_mars_boot_1000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_33964.R'

cv_lm_cv_1000_0.95_63935.RData: cv_lm_cv_1000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_63935.R'

cv_lm_boot_5000_0.9_90992.RData: cv_lm_boot_5000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_90992.R'

cv_mars_cv_5000_0.9_28082.RData: cv_mars_cv_5000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_28082.R'

cv_mlp_cv_1000_0.9_33964.RData: cv_mlp_cv_1000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_33964.R'

cv_mars_boot_500_0.9_70246.RData: cv_mars_boot_500_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_70246.R'

cv_lm_boot_1000_0.95_93901.RData: cv_lm_boot_1000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_93901.R'

cv_lm_cv_1000_0.9_63935.RData: cv_lm_cv_1000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_63935.R'

cv_mlp_boot_1000_0.95_69773.RData: cv_mlp_boot_1000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_69773.R'

cv_mlp_boot_1000_0.9_88428.RData: cv_mlp_boot_1000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_88428.R'

cv_mlp_cv_1000_0.95_61920.RData: cv_mlp_cv_1000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_61920.R'

cv_mlp_cv_500_0.9_33964.RData: cv_mlp_cv_500_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_33964.R'

cv_lm_cv_5000_0.95_28082.RData: cv_lm_cv_5000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_28082.R'

cv_mlp_boot_1000_0.9_85818.RData: cv_mlp_boot_1000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_85818.R'

cv_mars_boot_1000_0.95_65769.RData: cv_mars_boot_1000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_65769.R'

cv_lm_cv_500_0.9_9503.RData: cv_lm_cv_500_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_9503.R'

cv_mlp_boot_500_0.95_54686.RData: cv_mlp_boot_500_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_54686.R'

cv_mars_boot_500_0.9_61920.RData: cv_mars_boot_500_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_61920.R'

cv_mars_boot_500_0.9_69773.RData: cv_mars_boot_500_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_69773.R'

cv_mars_boot_5000_0.95_9503.RData: cv_mars_boot_5000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_9503.R'

cv_mlp_cv_500_0.9_93901.RData: cv_mlp_cv_500_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_93901.R'

cv_mlp_cv_5000_0.9_25462.RData: cv_mlp_cv_5000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_25462.R'

cv_lm_cv_1000_0.95_74917.RData: cv_lm_cv_1000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_74917.R'

cv_lm_cv_5000_0.95_90992.RData: cv_lm_cv_5000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_90992.R'

cv_lm_cv_1000_0.95_93901.RData: cv_lm_cv_1000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_93901.R'

cv_lm_boot_500_0.9_90992.RData: cv_lm_boot_500_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_90992.R'

cv_mars_boot_5000_0.9_93901.RData: cv_mars_boot_5000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_93901.R'

cv_mars_boot_5000_0.9_69773.RData: cv_mars_boot_5000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_69773.R'

cv_mars_cv_1000_0.9_18633.RData: cv_mars_cv_1000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_18633.R'

cv_mlp_boot_5000_0.95_88428.RData: cv_mlp_boot_5000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_88428.R'

cv_lm_cv_5000_0.95_65769.RData: cv_lm_cv_5000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_65769.R'

cv_mars_cv_1000_0.9_37035.RData: cv_mars_cv_1000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_37035.R'

cv_mars_cv_5000_0.95_90992.RData: cv_mars_cv_5000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_90992.R'

cv_mlp_boot_5000_0.95_63935.RData: cv_mlp_boot_5000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_63935.R'

cv_mars_cv_5000_0.95_85818.RData: cv_mars_cv_5000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_85818.R'

cv_mars_boot_500_0.9_52080.RData: cv_mars_boot_500_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_52080.R'

cv_mlp_boot_5000_0.95_11319.RData: cv_mlp_boot_5000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_11319.R'

cv_mlp_boot_5000_0.95_52080.RData: cv_mlp_boot_5000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_52080.R'

cv_mlp_boot_5000_0.9_38345.RData: cv_mlp_boot_5000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_38345.R'

cv_mars_cv_1000_0.9_94074.RData: cv_mars_cv_1000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_94074.R'

cv_lm_cv_5000_0.95_74917.RData: cv_lm_cv_5000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_74917.R'

cv_lm_boot_5000_0.9_93901.RData: cv_lm_boot_5000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_93901.R'

cv_mlp_cv_500_0.9_94074.RData: cv_mlp_cv_500_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_94074.R'

cv_lm_boot_1000_0.95_33964.RData: cv_lm_boot_1000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_33964.R'

cv_mlp_cv_500_0.9_70246.RData: cv_mlp_cv_500_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_70246.R'

cv_mlp_cv_500_0.9_90601.RData: cv_mlp_cv_500_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_90601.R'

cv_mlp_boot_1000_0.95_93901.RData: cv_mlp_boot_1000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_93901.R'

cv_lm_cv_1000_0.95_70246.RData: cv_lm_cv_1000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_70246.R'

cv_lm_cv_500_0.95_13308.RData: cv_lm_cv_500_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_13308.R'

cv_mlp_cv_1000_0.9_90601.RData: cv_mlp_cv_1000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_90601.R'

cv_mars_cv_1000_0.95_9503.RData: cv_mars_cv_1000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_9503.R'

cv_lm_boot_5000_0.9_38345.RData: cv_lm_boot_5000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_38345.R'

cv_lm_boot_500_0.95_33453.RData: cv_lm_boot_500_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_33453.R'

cv_mars_boot_500_0.95_11319.RData: cv_mars_boot_500_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_11319.R'

cv_lm_boot_1000_0.9_82586.RData: cv_lm_boot_1000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_82586.R'

cv_mars_cv_5000_0.9_25462.RData: cv_mars_cv_5000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_25462.R'

cv_mlp_cv_500_0.95_93901.RData: cv_mlp_cv_500_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_93901.R'

cv_mlp_cv_500_0.9_82586.RData: cv_mlp_cv_500_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_82586.R'

cv_mlp_boot_500_0.9_93901.RData: cv_mlp_boot_500_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_93901.R'

cv_mlp_boot_5000_0.9_13308.RData: cv_mlp_boot_5000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_13308.R'

cv_mlp_boot_1000_0.9_9503.RData: cv_mlp_boot_1000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_9503.R'

cv_mars_cv_500_0.95_54686.RData: cv_mars_cv_500_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_54686.R'

cv_mlp_boot_5000_0.9_61920.RData: cv_mlp_boot_5000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_61920.R'

cv_mars_boot_1000_0.9_54686.RData: cv_mars_boot_1000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_54686.R'

cv_mlp_boot_500_0.9_38345.RData: cv_mlp_boot_500_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_38345.R'

cv_mlp_cv_1000_0.9_70246.RData: cv_mlp_cv_1000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_70246.R'

cv_mars_cv_5000_0.95_69773.RData: cv_mars_cv_5000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_69773.R'

cv_lm_boot_5000_0.9_13308.RData: cv_lm_boot_5000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_13308.R'

cv_mlp_cv_1000_0.9_69773.RData: cv_mlp_cv_1000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_69773.R'

cv_lm_boot_500_0.9_28082.RData: cv_lm_boot_500_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_28082.R'

cv_mlp_cv_5000_0.95_25462.RData: cv_mlp_cv_5000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_25462.R'

cv_mlp_boot_500_0.95_18633.RData: cv_mlp_boot_500_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_18633.R'

cv_mars_cv_1000_0.9_54686.RData: cv_mars_cv_1000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_54686.R'

cv_mlp_cv_500_0.9_18633.RData: cv_mlp_cv_500_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_18633.R'

cv_mlp_boot_5000_0.9_90601.RData: cv_mlp_boot_5000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_90601.R'

cv_lm_boot_500_0.95_88428.RData: cv_lm_boot_500_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_88428.R'

cv_lm_boot_5000_0.9_18633.RData: cv_lm_boot_5000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_18633.R'

cv_mars_cv_5000_0.95_38345.RData: cv_mars_cv_5000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_38345.R'

cv_mlp_cv_500_0.9_54686.RData: cv_mlp_cv_500_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_54686.R'

cv_lm_boot_5000_0.9_65769.RData: cv_lm_boot_5000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_65769.R'

cv_mars_cv_1000_0.95_25462.RData: cv_mars_cv_1000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_25462.R'

cv_mlp_boot_500_0.95_9503.RData: cv_mlp_boot_500_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_9503.R'

cv_lm_cv_1000_0.9_93901.RData: cv_lm_cv_1000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_93901.R'

cv_lm_cv_1000_0.9_90992.RData: cv_lm_cv_1000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_90992.R'

cv_lm_boot_1000_0.9_33453.RData: cv_lm_boot_1000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_33453.R'

cv_mars_cv_500_0.95_61920.RData: cv_mars_cv_500_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_61920.R'

cv_mars_boot_500_0.9_90992.RData: cv_mars_boot_500_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_90992.R'

cv_mars_boot_5000_0.95_90992.RData: cv_mars_boot_5000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_90992.R'

cv_mlp_boot_1000_0.95_88428.RData: cv_mlp_boot_1000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_88428.R'

cv_lm_cv_1000_0.95_54686.RData: cv_lm_cv_1000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_54686.R'

cv_mars_boot_500_0.95_52080.RData: cv_mars_boot_500_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_52080.R'

cv_mars_boot_1000_0.9_61920.RData: cv_mars_boot_1000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_61920.R'

cv_mars_cv_500_0.95_33964.RData: cv_mars_cv_500_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_33964.R'

cv_lm_boot_500_0.9_11319.RData: cv_lm_boot_500_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_11319.R'

cv_mlp_boot_5000_0.9_63935.RData: cv_mlp_boot_5000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_63935.R'

cv_mars_cv_500_0.95_85818.RData: cv_mars_cv_500_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_85818.R'

cv_mars_boot_500_0.95_28082.RData: cv_mars_boot_500_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_28082.R'

cv_mars_boot_1000_0.95_90992.RData: cv_mars_boot_1000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_90992.R'

cv_mlp_boot_5000_0.95_69773.RData: cv_mlp_boot_5000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_69773.R'

cv_lm_boot_1000_0.9_25462.RData: cv_lm_boot_1000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_25462.R'

cv_lm_boot_500_0.9_33964.RData: cv_lm_boot_500_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_33964.R'

cv_mars_cv_1000_0.95_70246.RData: cv_mars_cv_1000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_70246.R'

cv_lm_boot_1000_0.9_63935.RData: cv_lm_boot_1000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_63935.R'

cv_mlp_cv_500_0.95_61920.RData: cv_mlp_cv_500_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_61920.R'

cv_mars_boot_500_0.9_93901.RData: cv_mars_boot_500_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_93901.R'

cv_mlp_cv_5000_0.95_63935.RData: cv_mlp_cv_5000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_63935.R'

cv_mlp_cv_5000_0.95_90992.RData: cv_mlp_cv_5000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_90992.R'

cv_mars_cv_5000_0.95_74917.RData: cv_mars_cv_5000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_74917.R'

cv_lm_cv_5000_0.95_9503.RData: cv_lm_cv_5000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_9503.R'

cv_mars_boot_5000_0.95_88428.RData: cv_mars_boot_5000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_88428.R'

cv_mars_cv_5000_0.9_11319.RData: cv_mars_cv_5000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_11319.R'

cv_lm_cv_5000_0.9_25462.RData: cv_lm_cv_5000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_25462.R'

cv_mars_cv_500_0.9_9503.RData: cv_mars_cv_500_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_9503.R'

cv_lm_cv_1000_0.9_33453.RData: cv_lm_cv_1000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_33453.R'

cv_mlp_boot_5000_0.95_65769.RData: cv_mlp_boot_5000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_65769.R'

cv_mars_cv_5000_0.95_54686.RData: cv_mars_cv_5000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_54686.R'

cv_mars_boot_5000_0.9_94074.RData: cv_mars_boot_5000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_94074.R'

cv_lm_boot_500_0.9_38345.RData: cv_lm_boot_500_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_38345.R'

cv_mars_cv_500_0.9_52080.RData: cv_mars_cv_500_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_52080.R'

cv_mars_cv_1000_0.95_88428.RData: cv_mars_cv_1000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_88428.R'

cv_lm_cv_1000_0.95_90992.RData: cv_lm_cv_1000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_90992.R'

cv_lm_cv_500_0.95_52080.RData: cv_lm_cv_500_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_52080.R'

cv_lm_cv_5000_0.9_85818.RData: cv_lm_cv_5000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_85818.R'

cv_mars_cv_1000_0.9_25462.RData: cv_mars_cv_1000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_25462.R'

cv_mlp_boot_1000_0.95_52080.RData: cv_mlp_boot_1000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_52080.R'

cv_mlp_cv_5000_0.9_70246.RData: cv_mlp_cv_5000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_70246.R'

cv_mlp_boot_500_0.9_28082.RData: cv_mlp_boot_500_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_28082.R'

cv_lm_boot_1000_0.95_25462.RData: cv_lm_boot_1000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_25462.R'

cv_lm_cv_500_0.9_11319.RData: cv_lm_cv_500_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_11319.R'

cv_mlp_boot_1000_0.9_52080.RData: cv_mlp_boot_1000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_52080.R'

cv_lm_cv_500_0.95_82586.RData: cv_lm_cv_500_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_82586.R'

cv_mlp_boot_500_0.95_69773.RData: cv_mlp_boot_500_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_69773.R'

cv_lm_boot_500_0.9_25462.RData: cv_lm_boot_500_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_25462.R'

cv_lm_boot_5000_0.9_11319.RData: cv_lm_boot_5000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_11319.R'

cv_lm_cv_5000_0.9_90992.RData: cv_lm_cv_5000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_90992.R'

cv_mlp_cv_5000_0.9_61920.RData: cv_mlp_cv_5000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_61920.R'

cv_lm_boot_1000_0.95_28082.RData: cv_lm_boot_1000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_28082.R'

cv_lm_boot_1000_0.95_63935.RData: cv_lm_boot_1000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_63935.R'

cv_lm_cv_5000_0.9_28082.RData: cv_lm_cv_5000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_28082.R'

cv_lm_cv_1000_0.9_18633.RData: cv_lm_cv_1000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_18633.R'

cv_mars_boot_1000_0.95_25462.RData: cv_mars_boot_1000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_25462.R'

cv_lm_cv_1000_0.95_18633.RData: cv_lm_cv_1000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_18633.R'

cv_mars_boot_5000_0.9_90992.RData: cv_mars_boot_5000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_90992.R'

cv_mars_cv_1000_0.95_54686.RData: cv_mars_cv_1000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_54686.R'

cv_mars_cv_5000_0.95_63935.RData: cv_mars_cv_5000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_63935.R'

cv_mars_cv_5000_0.9_54686.RData: cv_mars_cv_5000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_54686.R'

cv_mlp_boot_5000_0.9_18633.RData: cv_mlp_boot_5000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_18633.R'

cv_lm_boot_500_0.95_94074.RData: cv_lm_boot_500_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_94074.R'

cv_mlp_cv_5000_0.95_82586.RData: cv_mlp_cv_5000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_82586.R'

cv_mars_boot_5000_0.9_65769.RData: cv_mars_boot_5000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_65769.R'

cv_mlp_cv_5000_0.9_37035.RData: cv_mlp_cv_5000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_37035.R'

cv_lm_cv_5000_0.9_63935.RData: cv_lm_cv_5000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_63935.R'

cv_lm_boot_500_0.95_85818.RData: cv_lm_boot_500_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_85818.R'

cv_lm_boot_1000_0.95_61920.RData: cv_lm_boot_1000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_61920.R'

cv_mars_cv_5000_0.95_52080.RData: cv_mars_cv_5000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_52080.R'

cv_lm_boot_5000_0.95_61920.RData: cv_lm_boot_5000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_61920.R'

cv_mars_cv_500_0.95_90601.RData: cv_mars_cv_500_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_90601.R'

cv_mlp_boot_500_0.95_85818.RData: cv_mlp_boot_500_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_85818.R'

cv_mars_boot_1000_0.9_28082.RData: cv_mars_boot_1000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_28082.R'

cv_mlp_cv_5000_0.9_94074.RData: cv_mlp_cv_5000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_94074.R'

cv_mlp_boot_500_0.9_85818.RData: cv_mlp_boot_500_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_85818.R'

cv_mlp_boot_500_0.95_65769.RData: cv_mlp_boot_500_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_65769.R'

cv_lm_boot_1000_0.9_93901.RData: cv_lm_boot_1000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_93901.R'

cv_mars_boot_1000_0.9_33453.RData: cv_mars_boot_1000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_33453.R'

cv_mlp_cv_5000_0.9_13308.RData: cv_mlp_cv_5000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_13308.R'

cv_mlp_boot_1000_0.9_33964.RData: cv_mlp_boot_1000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_33964.R'

cv_mars_boot_1000_0.9_33964.RData: cv_mars_boot_1000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_33964.R'

cv_lm_boot_500_0.9_52080.RData: cv_lm_boot_500_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_52080.R'

cv_lm_boot_500_0.95_9503.RData: cv_lm_boot_500_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_9503.R'

cv_lm_boot_5000_0.95_28082.RData: cv_lm_boot_5000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_28082.R'

cv_mlp_boot_5000_0.95_25462.RData: cv_mlp_boot_5000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_25462.R'

cv_lm_cv_5000_0.95_90601.RData: cv_lm_cv_5000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_90601.R'

cv_mlp_cv_500_0.9_25462.RData: cv_mlp_cv_500_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_25462.R'

cv_lm_boot_5000_0.95_90601.RData: cv_lm_boot_5000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_90601.R'

cv_lm_cv_500_0.9_33453.RData: cv_lm_cv_500_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_33453.R'

cv_mars_boot_500_0.9_13308.RData: cv_mars_boot_500_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_13308.R'

cv_mars_boot_5000_0.9_28082.RData: cv_mars_boot_5000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_28082.R'

cv_lm_boot_5000_0.9_82586.RData: cv_lm_boot_5000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_82586.R'

cv_mlp_boot_5000_0.9_28082.RData: cv_mlp_boot_5000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_28082.R'

cv_lm_cv_1000_0.95_13308.RData: cv_lm_cv_1000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_13308.R'

cv_mlp_cv_1000_0.9_74917.RData: cv_mlp_cv_1000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_74917.R'

cv_lm_cv_1000_0.95_33453.RData: cv_lm_cv_1000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_33453.R'

cv_lm_boot_500_0.95_93901.RData: cv_lm_boot_500_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_93901.R'

cv_mars_cv_5000_0.95_90601.RData: cv_mars_cv_5000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_90601.R'

cv_mlp_cv_1000_0.95_69773.RData: cv_mlp_cv_1000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_69773.R'

cv_lm_cv_500_0.95_69773.RData: cv_lm_cv_500_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_69773.R'

cv_mars_boot_500_0.95_63935.RData: cv_mars_boot_500_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_63935.R'

cv_lm_boot_5000_0.9_70246.RData: cv_lm_boot_5000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_70246.R'

cv_lm_cv_500_0.95_61920.RData: cv_lm_cv_500_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_61920.R'

cv_mars_cv_1000_0.95_74917.RData: cv_mars_cv_1000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_74917.R'

cv_mlp_boot_1000_0.95_33964.RData: cv_mlp_boot_1000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_33964.R'

cv_lm_boot_500_0.9_70246.RData: cv_lm_boot_500_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_70246.R'

cv_mars_cv_5000_0.9_90992.RData: cv_mars_cv_5000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_90992.R'

cv_lm_boot_500_0.95_61920.RData: cv_lm_boot_500_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_61920.R'

cv_mlp_cv_5000_0.95_69773.RData: cv_mlp_cv_5000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_69773.R'

cv_lm_boot_5000_0.95_11319.RData: cv_lm_boot_5000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_11319.R'

cv_mars_cv_1000_0.9_93901.RData: cv_mars_cv_1000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_93901.R'

cv_mars_boot_500_0.95_93901.RData: cv_mars_boot_500_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_93901.R'

cv_lm_boot_500_0.95_13308.RData: cv_lm_boot_500_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_13308.R'

cv_mlp_cv_500_0.95_28082.RData: cv_mlp_cv_500_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_28082.R'

cv_mlp_boot_5000_0.95_33964.RData: cv_mlp_boot_5000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_33964.R'

cv_mars_boot_1000_0.9_11319.RData: cv_mars_boot_1000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_11319.R'

cv_mlp_boot_5000_0.9_33453.RData: cv_mlp_boot_5000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_33453.R'

cv_mlp_boot_5000_0.9_65769.RData: cv_mlp_boot_5000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_65769.R'

cv_mlp_boot_500_0.9_63935.RData: cv_mlp_boot_500_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_63935.R'

cv_lm_cv_500_0.9_85818.RData: cv_lm_cv_500_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_85818.R'

cv_mars_boot_5000_0.9_33453.RData: cv_mars_boot_5000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_33453.R'

cv_mlp_cv_1000_0.95_25462.RData: cv_mlp_cv_1000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_25462.R'

cv_mars_boot_1000_0.9_74917.RData: cv_mars_boot_1000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_74917.R'

cv_mars_cv_5000_0.9_37035.RData: cv_mars_cv_5000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_37035.R'

cv_mlp_cv_1000_0.95_18633.RData: cv_mlp_cv_1000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_18633.R'

cv_mars_cv_5000_0.9_63935.RData: cv_mars_cv_5000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_63935.R'

cv_lm_boot_500_0.95_11319.RData: cv_lm_boot_500_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_11319.R'

cv_mlp_boot_500_0.9_54686.RData: cv_mlp_boot_500_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_54686.R'

cv_mlp_cv_1000_0.9_61920.RData: cv_mlp_cv_1000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_61920.R'

cv_lm_cv_5000_0.95_70246.RData: cv_lm_cv_5000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_70246.R'

cv_mlp_boot_5000_0.95_13308.RData: cv_mlp_boot_5000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_13308.R'

cv_lm_boot_5000_0.9_88428.RData: cv_lm_boot_5000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_88428.R'

cv_mars_boot_5000_0.95_85818.RData: cv_mars_boot_5000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_85818.R'

cv_mars_cv_1000_0.9_28082.RData: cv_mars_cv_1000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_28082.R'

cv_mars_boot_5000_0.95_65769.RData: cv_mars_boot_5000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_65769.R'

cv_mlp_boot_5000_0.9_11319.RData: cv_mlp_boot_5000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_11319.R'

cv_mars_boot_500_0.9_74917.RData: cv_mars_boot_500_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_74917.R'

cv_mlp_cv_500_0.9_52080.RData: cv_mlp_cv_500_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_52080.R'

cv_lm_boot_5000_0.95_33964.RData: cv_lm_boot_5000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_33964.R'

cv_lm_boot_500_0.95_38345.RData: cv_lm_boot_500_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_38345.R'

cv_mars_cv_500_0.9_61920.RData: cv_mars_cv_500_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_61920.R'

cv_lm_cv_500_0.9_70246.RData: cv_lm_cv_500_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_70246.R'

cv_mlp_cv_500_0.95_52080.RData: cv_mlp_cv_500_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_52080.R'

cv_lm_boot_5000_0.9_90601.RData: cv_lm_boot_5000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_90601.R'

cv_lm_cv_5000_0.95_33453.RData: cv_lm_cv_5000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_33453.R'

cv_mlp_cv_1000_0.95_74917.RData: cv_mlp_cv_1000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_74917.R'

cv_mars_boot_500_0.95_38345.RData: cv_mars_boot_500_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_38345.R'

cv_mars_boot_1000_0.9_82586.RData: cv_mars_boot_1000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_82586.R'

cv_mars_cv_1000_0.95_33453.RData: cv_mars_cv_1000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_33453.R'

cv_lm_cv_5000_0.9_52080.RData: cv_lm_cv_5000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_52080.R'

cv_lm_cv_5000_0.9_94074.RData: cv_lm_cv_5000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_94074.R'

cv_mlp_boot_500_0.95_38345.RData: cv_mlp_boot_500_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_38345.R'

cv_mlp_boot_5000_0.9_82586.RData: cv_mlp_boot_5000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_82586.R'

cv_mlp_boot_1000_0.95_82586.RData: cv_mlp_boot_1000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_82586.R'

cv_mars_boot_1000_0.9_85818.RData: cv_mars_boot_1000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_85818.R'

cv_mars_cv_5000_0.95_33964.RData: cv_mars_cv_5000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_33964.R'

cv_mars_boot_500_0.9_88428.RData: cv_mars_boot_500_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_88428.R'

cv_mars_boot_5000_0.95_11319.RData: cv_mars_boot_5000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_11319.R'

cv_mars_cv_5000_0.9_38345.RData: cv_mars_cv_5000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_38345.R'

cv_mlp_cv_5000_0.9_9503.RData: cv_mlp_cv_5000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_9503.R'

cv_mars_boot_5000_0.95_94074.RData: cv_mars_boot_5000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_94074.R'

cv_lm_boot_500_0.9_33453.RData: cv_lm_boot_500_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_33453.R'

cv_lm_boot_500_0.9_82586.RData: cv_lm_boot_500_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_82586.R'

cv_lm_boot_5000_0.95_9503.RData: cv_lm_boot_5000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_9503.R'

cv_lm_boot_5000_0.9_37035.RData: cv_lm_boot_5000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_37035.R'

cv_mars_cv_1000_0.9_90601.RData: cv_mars_cv_1000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_90601.R'

cv_lm_boot_1000_0.9_74917.RData: cv_lm_boot_1000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_74917.R'

cv_mlp_boot_5000_0.95_74917.RData: cv_mlp_boot_5000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_74917.R'

cv_mars_boot_1000_0.9_90992.RData: cv_mars_boot_1000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_90992.R'

cv_mars_cv_500_0.95_70246.RData: cv_mars_cv_500_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_70246.R'

cv_mlp_boot_500_0.9_69773.RData: cv_mlp_boot_500_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_69773.R'

cv_mlp_boot_500_0.9_90992.RData: cv_mlp_boot_500_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_90992.R'

cv_mlp_boot_500_0.95_70246.RData: cv_mlp_boot_500_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_70246.R'

cv_mars_boot_500_0.9_18633.RData: cv_mars_boot_500_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_18633.R'

cv_lm_boot_500_0.95_18633.RData: cv_lm_boot_500_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_18633.R'

cv_lm_cv_1000_0.9_28082.RData: cv_lm_cv_1000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_28082.R'

cv_mars_boot_5000_0.9_25462.RData: cv_mars_boot_5000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_25462.R'

cv_mlp_boot_5000_0.9_70246.RData: cv_mlp_boot_5000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_70246.R'

cv_lm_boot_1000_0.95_54686.RData: cv_lm_boot_1000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_54686.R'

cv_mars_boot_500_0.95_65769.RData: cv_mars_boot_500_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_65769.R'

cv_lm_boot_500_0.95_74917.RData: cv_lm_boot_500_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_74917.R'

cv_mars_boot_5000_0.9_33964.RData: cv_mars_boot_5000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_33964.R'

cv_mlp_cv_500_0.95_74917.RData: cv_mlp_cv_500_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_74917.R'

cv_mlp_boot_1000_0.95_94074.RData: cv_mlp_boot_1000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_94074.R'

cv_lm_boot_1000_0.95_18633.RData: cv_lm_boot_1000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_18633.R'

cv_mlp_cv_1000_0.9_65769.RData: cv_mlp_cv_1000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_65769.R'

cv_mlp_cv_500_0.9_28082.RData: cv_mlp_cv_500_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_28082.R'

cv_lm_cv_500_0.9_33964.RData: cv_lm_cv_500_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_33964.R'

cv_mlp_cv_1000_0.95_33453.RData: cv_mlp_cv_1000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_33453.R'

cv_mars_boot_1000_0.9_25462.RData: cv_mars_boot_1000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_25462.R'

cv_mlp_cv_1000_0.95_54686.RData: cv_mlp_cv_1000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_54686.R'

cv_mlp_cv_500_0.95_9503.RData: cv_mlp_cv_500_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_9503.R'

cv_mars_cv_1000_0.9_70246.RData: cv_mars_cv_1000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_70246.R'

cv_mars_cv_500_0.95_13308.RData: cv_mars_cv_500_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_13308.R'

cv_lm_boot_500_0.9_13308.RData: cv_lm_boot_500_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_13308.R'

cv_lm_cv_5000_0.9_69773.RData: cv_lm_cv_5000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_69773.R'

cv_lm_cv_500_0.9_90992.RData: cv_lm_cv_500_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_90992.R'

cv_lm_boot_500_0.9_94074.RData: cv_lm_boot_500_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_94074.R'

cv_mars_cv_5000_0.95_70246.RData: cv_mars_cv_5000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_70246.R'

cv_mars_cv_1000_0.95_90601.RData: cv_mars_cv_1000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_90601.R'

cv_lm_cv_1000_0.9_9503.RData: cv_lm_cv_1000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_9503.R'

cv_lm_boot_5000_0.9_74917.RData: cv_lm_boot_5000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_74917.R'

cv_lm_cv_500_0.9_88428.RData: cv_lm_cv_500_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_88428.R'

cv_mlp_boot_5000_0.95_93901.RData: cv_mlp_boot_5000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_93901.R'

cv_mars_boot_500_0.95_88428.RData: cv_mars_boot_500_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_88428.R'

cv_mars_boot_5000_0.9_74917.RData: cv_mars_boot_5000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_74917.R'

cv_mars_boot_500_0.9_54686.RData: cv_mars_boot_500_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_54686.R'

cv_mars_boot_5000_0.95_28082.RData: cv_mars_boot_5000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_28082.R'

cv_mlp_boot_5000_0.9_88428.RData: cv_mlp_boot_5000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_88428.R'

cv_mars_boot_1000_0.9_37035.RData: cv_mars_boot_1000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_37035.R'

cv_lm_boot_1000_0.95_37035.RData: cv_lm_boot_1000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_37035.R'

cv_mlp_cv_5000_0.9_93901.RData: cv_mlp_cv_5000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_93901.R'

cv_mlp_boot_5000_0.9_37035.RData: cv_mlp_boot_5000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_37035.R'

cv_mars_cv_1000_0.95_93901.RData: cv_mars_cv_1000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_93901.R'

cv_mars_cv_1000_0.95_90992.RData: cv_mars_cv_1000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_90992.R'

cv_mars_boot_5000_0.9_85818.RData: cv_mars_boot_5000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_85818.R'

cv_mlp_boot_1000_0.95_18633.RData: cv_mlp_boot_1000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_18633.R'

cv_mars_cv_1000_0.9_33964.RData: cv_mars_cv_1000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_33964.R'

cv_mars_boot_1000_0.95_33453.RData: cv_mars_boot_1000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_33453.R'

cv_mlp_cv_500_0.95_90601.RData: cv_mlp_cv_500_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_90601.R'

cv_mlp_cv_5000_0.95_90601.RData: cv_mlp_cv_5000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_90601.R'

cv_mlp_boot_5000_0.95_38345.RData: cv_mlp_boot_5000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_38345.R'

cv_lm_boot_500_0.9_88428.RData: cv_lm_boot_500_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_88428.R'

cv_mlp_boot_1000_0.9_38345.RData: cv_mlp_boot_1000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_38345.R'

cv_mars_cv_5000_0.95_9503.RData: cv_mars_cv_5000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_9503.R'

cv_mars_boot_5000_0.95_93901.RData: cv_mars_boot_5000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_93901.R'

cv_mlp_cv_1000_0.95_28082.RData: cv_mlp_cv_1000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_28082.R'

cv_lm_boot_500_0.9_54686.RData: cv_lm_boot_500_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_54686.R'

cv_mars_cv_5000_0.95_33453.RData: cv_mars_cv_5000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_33453.R'

cv_lm_cv_5000_0.95_33964.RData: cv_lm_cv_5000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_33964.R'

cv_mars_boot_5000_0.9_70246.RData: cv_mars_boot_5000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_70246.R'

cv_mars_boot_500_0.9_90601.RData: cv_mars_boot_500_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_90601.R'

cv_mars_boot_1000_0.95_13308.RData: cv_mars_boot_1000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_13308.R'

cv_lm_cv_500_0.95_90992.RData: cv_lm_cv_500_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_90992.R'

cv_lm_boot_5000_0.95_63935.RData: cv_lm_boot_5000_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_63935.R'

cv_mlp_cv_500_0.9_33453.RData: cv_mlp_cv_500_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_33453.R'

cv_mars_cv_5000_0.9_9503.RData: cv_mars_cv_5000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_9503.R'

cv_mars_boot_1000_0.95_52080.RData: cv_mars_boot_1000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_52080.R'

cv_mlp_boot_1000_0.9_33453.RData: cv_mlp_boot_1000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_33453.R'

cv_mars_cv_500_0.9_33453.RData: cv_mars_cv_500_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_33453.R'

cv_mars_cv_500_0.9_63935.RData: cv_mars_cv_500_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_63935.R'

cv_mlp_boot_500_0.9_33964.RData: cv_mlp_boot_500_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_33964.R'

cv_lm_cv_5000_0.9_90601.RData: cv_lm_cv_5000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_90601.R'

cv_lm_boot_500_0.95_65769.RData: cv_lm_boot_500_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_65769.R'

cv_mars_cv_500_0.9_69773.RData: cv_mars_cv_500_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_69773.R'

cv_mars_boot_1000_0.95_9503.RData: cv_mars_boot_1000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_9503.R'

cv_mars_boot_5000_0.95_90601.RData: cv_mars_boot_5000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_90601.R'

cv_lm_cv_500_0.95_38345.RData: cv_lm_cv_500_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_38345.R'

cv_mars_cv_5000_0.9_13308.RData: cv_mars_cv_5000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_13308.R'

cv_mlp_cv_5000_0.9_52080.RData: cv_mlp_cv_5000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_52080.R'

cv_mlp_cv_500_0.95_82586.RData: cv_mlp_cv_500_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_82586.R'

cv_mars_cv_500_0.9_18633.RData: cv_mars_cv_500_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_18633.R'

cv_lm_cv_1000_0.9_38345.RData: cv_lm_cv_1000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_38345.R'

cv_mars_cv_500_0.9_25462.RData: cv_mars_cv_500_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_25462.R'

cv_lm_cv_5000_0.9_82586.RData: cv_lm_cv_5000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_82586.R'

cv_mlp_boot_5000_0.9_9503.RData: cv_mlp_boot_5000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_9503.R'

cv_lm_cv_500_0.95_33964.RData: cv_lm_cv_500_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_33964.R'

cv_mars_cv_500_0.95_25462.RData: cv_mars_cv_500_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_25462.R'

cv_mars_cv_5000_0.9_70246.RData: cv_mars_cv_5000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_70246.R'

cv_mars_boot_5000_0.95_74917.RData: cv_mars_boot_5000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_74917.R'

cv_mlp_boot_5000_0.95_70246.RData: cv_mlp_boot_5000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_70246.R'

cv_mars_cv_1000_0.9_11319.RData: cv_mars_cv_1000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_11319.R'

cv_mars_boot_1000_0.95_54686.RData: cv_mars_boot_1000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_54686.R'

cv_mars_cv_5000_0.9_18633.RData: cv_mars_cv_5000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_18633.R'

cv_mlp_boot_5000_0.95_61920.RData: cv_mlp_boot_5000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_61920.R'

cv_mars_boot_5000_0.9_63935.RData: cv_mars_boot_5000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_63935.R'

cv_mlp_boot_5000_0.95_37035.RData: cv_mlp_boot_5000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_37035.R'

cv_mlp_cv_1000_0.95_9503.RData: cv_mlp_cv_1000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_9503.R'

cv_lm_cv_1000_0.95_69773.RData: cv_lm_cv_1000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_69773.R'

cv_mars_cv_500_0.9_90992.RData: cv_mars_cv_500_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_90992.R'

cv_mlp_boot_1000_0.9_74917.RData: cv_mlp_boot_1000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_74917.R'

cv_mlp_boot_1000_0.9_90992.RData: cv_mlp_boot_1000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_90992.R'

cv_mlp_boot_500_0.95_52080.RData: cv_mlp_boot_500_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_52080.R'

cv_lm_cv_500_0.9_25462.RData: cv_lm_cv_500_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_25462.R'

cv_mars_cv_5000_0.95_11319.RData: cv_mars_cv_5000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_11319.R'

cv_mlp_boot_5000_0.95_28082.RData: cv_mlp_boot_5000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_28082.R'

cv_mars_boot_5000_0.95_38345.RData: cv_mars_boot_5000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_38345.R'

cv_lm_cv_500_0.9_61920.RData: cv_lm_cv_500_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_61920.R'

cv_lm_boot_5000_0.9_33964.RData: cv_lm_boot_5000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_33964.R'

cv_mlp_cv_1000_0.95_93901.RData: cv_mlp_cv_1000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_93901.R'

cv_mars_boot_5000_0.95_61920.RData: cv_mars_boot_5000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_61920.R'

cv_lm_cv_1000_0.9_70246.RData: cv_lm_cv_1000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_70246.R'

cv_mars_boot_500_0.9_9503.RData: cv_mars_boot_500_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_9503.R'

cv_mlp_boot_5000_0.9_25462.RData: cv_mlp_boot_5000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_25462.R'

cv_lm_cv_500_0.95_63935.RData: cv_lm_cv_500_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_63935.R'

cv_mlp_cv_5000_0.95_94074.RData: cv_mlp_cv_5000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_94074.R'

cv_lm_boot_5000_0.95_38345.RData: cv_lm_boot_5000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_38345.R'

cv_mlp_boot_1000_0.95_74917.RData: cv_mlp_boot_1000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_74917.R'

cv_mars_boot_1000_0.9_69773.RData: cv_mars_boot_1000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_69773.R'

cv_lm_cv_5000_0.9_18633.RData: cv_lm_cv_5000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_18633.R'

cv_mlp_cv_1000_0.95_33964.RData: cv_mlp_cv_1000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_33964.R'

cv_lm_cv_1000_0.9_25462.RData: cv_lm_cv_1000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_25462.R'

cv_mlp_boot_5000_0.95_90992.RData: cv_mlp_boot_5000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_90992.R'

cv_lm_boot_1000_0.9_54686.RData: cv_lm_boot_1000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_54686.R'

cv_mlp_cv_5000_0.9_33453.RData: cv_mlp_cv_5000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_33453.R'

cv_mlp_boot_500_0.9_70246.RData: cv_mlp_boot_500_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_70246.R'

cv_mlp_boot_500_0.95_61920.RData: cv_mlp_boot_500_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_61920.R'

cv_mlp_cv_1000_0.9_54686.RData: cv_mlp_cv_1000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_54686.R'

cv_lm_cv_500_0.95_93901.RData: cv_lm_cv_500_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_93901.R'

cv_mars_boot_1000_0.95_74917.RData: cv_mars_boot_1000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_74917.R'

cv_mars_cv_5000_0.95_18633.RData: cv_mars_cv_5000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_18633.R'

cv_mlp_boot_1000_0.9_90601.RData: cv_mlp_boot_1000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_90601.R'

cv_mlp_boot_500_0.95_90992.RData: cv_mlp_boot_500_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_90992.R'

cv_lm_cv_5000_0.9_93901.RData: cv_lm_cv_5000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_93901.R'

cv_lm_cv_1000_0.95_38345.RData: cv_lm_cv_1000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_38345.R'

cv_mlp_cv_1000_0.9_28082.RData: cv_mlp_cv_1000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_28082.R'

cv_mars_cv_500_0.95_37035.RData: cv_mars_cv_500_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_37035.R'

cv_mars_boot_500_0.9_33453.RData: cv_mars_boot_500_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_33453.R'

cv_mlp_cv_5000_0.95_88428.RData: cv_mlp_cv_5000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_88428.R'

cv_lm_cv_500_0.9_18633.RData: cv_lm_cv_500_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_18633.R'

cv_mlp_cv_5000_0.95_33964.RData: cv_mlp_cv_5000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_33964.R'

cv_mlp_boot_1000_0.95_9503.RData: cv_mlp_boot_1000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_9503.R'

cv_mlp_boot_1000_0.95_38345.RData: cv_mlp_boot_1000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_38345.R'

cv_mlp_boot_500_0.95_94074.RData: cv_mlp_boot_500_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_94074.R'

cv_mlp_cv_5000_0.95_85818.RData: cv_mlp_cv_5000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_85818.R'

cv_mars_cv_5000_0.9_65769.RData: cv_mars_cv_5000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_65769.R'

cv_lm_boot_5000_0.9_25462.RData: cv_lm_boot_5000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_25462.R'

cv_mlp_cv_1000_0.9_63935.RData: cv_mlp_cv_1000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_63935.R'

cv_lm_boot_1000_0.95_74917.RData: cv_lm_boot_1000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_74917.R'

cv_mlp_boot_1000_0.95_33453.RData: cv_mlp_boot_1000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_33453.R'

cv_mars_boot_500_0.95_70246.RData: cv_mars_boot_500_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_70246.R'

cv_lm_boot_1000_0.95_90601.RData: cv_lm_boot_1000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_90601.R'

cv_lm_cv_5000_0.9_65769.RData: cv_lm_cv_5000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_65769.R'

cv_mars_cv_500_0.95_93901.RData: cv_mars_cv_500_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_93901.R'

cv_mars_boot_1000_0.95_90601.RData: cv_mars_boot_1000_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_90601.R'

cv_lm_boot_1000_0.9_28082.RData: cv_lm_boot_1000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_28082.R'

cv_lm_boot_500_0.9_69773.RData: cv_lm_boot_500_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_69773.R'

cv_lm_cv_5000_0.9_88428.RData: cv_lm_cv_5000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_88428.R'

cv_lm_boot_5000_0.95_69773.RData: cv_lm_boot_5000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_69773.R'

cv_lm_cv_500_0.95_25462.RData: cv_lm_cv_500_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_25462.R'

cv_lm_boot_1000_0.95_69773.RData: cv_lm_boot_1000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_69773.R'

cv_mars_cv_500_0.95_9503.RData: cv_mars_cv_500_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_9503.R'

cv_mars_cv_1000_0.9_63935.RData: cv_mars_cv_1000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_63935.R'

cv_mlp_boot_5000_0.95_82586.RData: cv_mlp_boot_5000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.95_82586.R'

cv_lm_boot_1000_0.95_82586.RData: cv_lm_boot_1000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_82586.R'

cv_mlp_boot_5000_0.9_33964.RData: cv_mlp_boot_5000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_33964.R'

cv_mlp_cv_1000_0.9_90992.RData: cv_mlp_cv_1000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_90992.R'

cv_mlp_cv_500_0.95_33964.RData: cv_mlp_cv_500_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_33964.R'

cv_mlp_boot_1000_0.9_37035.RData: cv_mlp_boot_1000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_37035.R'

cv_mars_boot_500_0.95_90601.RData: cv_mars_boot_500_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_90601.R'

cv_lm_cv_5000_0.95_94074.RData: cv_lm_cv_5000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_94074.R'

cv_mars_boot_1000_0.9_18633.RData: cv_mars_boot_1000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_18633.R'

cv_mlp_cv_5000_0.9_90601.RData: cv_mlp_cv_5000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_90601.R'

cv_lm_boot_1000_0.95_88428.RData: cv_lm_boot_1000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_88428.R'

cv_lm_cv_500_0.9_28082.RData: cv_lm_cv_500_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_28082.R'

cv_lm_boot_500_0.9_9503.RData: cv_lm_boot_500_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_9503.R'

cv_mars_cv_1000_0.95_33964.RData: cv_mars_cv_1000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_33964.R'

cv_lm_cv_1000_0.95_61920.RData: cv_lm_cv_1000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_61920.R'

cv_mlp_boot_500_0.95_74917.RData: cv_mlp_boot_500_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_74917.R'

cv_mars_boot_1000_0.9_65769.RData: cv_mars_boot_1000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_65769.R'

cv_mlp_boot_500_0.9_94074.RData: cv_mlp_boot_500_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_94074.R'

cv_lm_boot_1000_0.9_13308.RData: cv_lm_boot_1000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_13308.R'

cv_lm_cv_5000_0.9_11319.RData: cv_lm_cv_5000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_11319.R'

cv_mars_cv_1000_0.9_38345.RData: cv_mars_cv_1000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_38345.R'

cv_mars_boot_5000_0.9_61920.RData: cv_mars_boot_5000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_61920.R'

cv_mars_cv_1000_0.95_65769.RData: cv_mars_cv_1000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_65769.R'

cv_mars_boot_5000_0.9_38345.RData: cv_mars_boot_5000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_38345.R'

cv_mars_cv_500_0.95_11319.RData: cv_mars_cv_500_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_11319.R'

cv_mlp_cv_500_0.9_85818.RData: cv_mlp_cv_500_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_85818.R'

cv_mlp_boot_500_0.95_13308.RData: cv_mlp_boot_500_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_13308.R'

cv_mars_cv_500_0.9_85818.RData: cv_mars_cv_500_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_85818.R'

cv_lm_boot_5000_0.95_74917.RData: cv_lm_boot_5000_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_74917.R'

cv_lm_boot_1000_0.9_52080.RData: cv_lm_boot_1000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_52080.R'

cv_mars_cv_5000_0.9_52080.RData: cv_mars_cv_5000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_52080.R'

cv_lm_cv_500_0.9_94074.RData: cv_lm_cv_500_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_94074.R'

cv_mars_cv_500_0.9_28082.RData: cv_mars_cv_500_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_28082.R'

cv_mlp_boot_5000_0.9_93901.RData: cv_mlp_boot_5000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_93901.R'

cv_mlp_cv_500_0.95_94074.RData: cv_mlp_cv_500_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_94074.R'

cv_mars_cv_5000_0.9_85818.RData: cv_mars_cv_5000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_85818.R'

cv_lm_cv_1000_0.95_37035.RData: cv_lm_cv_1000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_37035.R'

cv_mars_boot_500_0.95_25462.RData: cv_mars_boot_500_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_25462.R'

cv_mars_cv_1000_0.9_13308.RData: cv_mars_cv_1000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_13308.R'

cv_mlp_boot_1000_0.95_11319.RData: cv_mlp_boot_1000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_11319.R'

cv_lm_cv_5000_0.95_93901.RData: cv_lm_cv_5000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_93901.R'

cv_lm_boot_1000_0.9_90601.RData: cv_lm_boot_1000_0.9_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_90601.R'

cv_mars_cv_5000_0.95_37035.RData: cv_mars_cv_5000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_37035.R'

cv_mars_cv_5000_0.9_74917.RData: cv_mars_cv_5000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_74917.R'

cv_lm_cv_5000_0.9_9503.RData: cv_lm_cv_5000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_9503.R'

cv_mlp_cv_5000_0.9_65769.RData: cv_mlp_cv_5000_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_65769.R'

cv_mars_cv_500_0.9_38345.RData: cv_mars_cv_500_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_38345.R'

cv_mlp_cv_5000_0.95_9503.RData: cv_mlp_cv_5000_0.95_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_9503.R'

cv_mars_boot_500_0.95_37035.RData: cv_mars_boot_500_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_37035.R'

cv_mars_boot_500_0.9_33964.RData: cv_mars_boot_500_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_33964.R'

cv_lm_cv_1000_0.9_88428.RData: cv_lm_cv_1000_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_88428.R'

cv_lm_cv_5000_0.95_37035.RData: cv_lm_cv_5000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_37035.R'

cv_mars_cv_5000_0.9_93901.RData: cv_mars_cv_5000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_93901.R'

cv_mlp_boot_1000_0.9_61920.RData: cv_mlp_boot_1000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_61920.R'

cv_lm_boot_5000_0.95_13308.RData: cv_lm_boot_5000_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_13308.R'

cv_mlp_cv_500_0.9_69773.RData: cv_mlp_cv_500_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.9_69773.R'

cv_mars_cv_5000_0.95_65769.RData: cv_mars_cv_5000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_65769.R'

cv_mlp_cv_1000_0.9_52080.RData: cv_mlp_cv_1000_0.9_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_52080.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_52080.R'

cv_mlp_cv_5000_0.9_74917.RData: cv_mlp_cv_5000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_74917.R'

cv_mars_cv_5000_0.9_82586.RData: cv_mars_cv_5000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.9_82586.R'

cv_mars_boot_5000_0.9_37035.RData: cv_mars_boot_5000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_37035.R'

cv_mlp_boot_5000_0.9_94074.RData: cv_mlp_boot_5000_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_94074.R'

cv_mars_boot_5000_0.9_82586.RData: cv_mars_boot_5000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_82586.R'

cv_mars_cv_500_0.95_28082.RData: cv_mars_cv_500_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.95_28082.R'

cv_mars_cv_1000_0.9_90992.RData: cv_mars_cv_1000_0.9_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_90992.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_90992.R'

cv_lm_cv_1000_0.95_94074.RData: cv_lm_cv_1000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_94074.R'

cv_mlp_cv_5000_0.9_38345.RData: cv_mlp_cv_5000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_38345.R'

cv_mars_boot_5000_0.95_18633.RData: cv_mars_boot_5000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_18633.R'

cv_mlp_cv_5000_0.95_70246.RData: cv_mlp_cv_5000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_70246.R'

cv_mars_cv_5000_0.95_61920.RData: cv_mars_cv_5000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_61920.R'

cv_mlp_boot_1000_0.9_70246.RData: cv_mlp_boot_1000_0.9_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_70246.R'

cv_mlp_cv_1000_0.9_37035.RData: cv_mlp_cv_1000_0.9_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_37035.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_37035.R'

cv_mars_cv_500_0.9_74917.RData: cv_mars_cv_500_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_74917.R'

cv_mars_cv_1000_0.95_52080.RData: cv_mars_cv_1000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_52080.R'

cv_mars_boot_1000_0.9_93901.RData: cv_mars_boot_1000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.9_93901.R'

cv_mars_cv_1000_0.95_61920.RData: cv_mars_cv_1000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_61920.R'

cv_mlp_cv_1000_0.9_38345.RData: cv_mlp_cv_1000_0.9_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_38345.R'

cv_lm_cv_5000_0.95_69773.RData: cv_lm_cv_5000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_69773.R'

cv_mars_boot_1000_0.95_82586.RData: cv_mars_boot_1000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_82586.R'

cv_mlp_boot_500_0.9_9503.RData: cv_mlp_boot_500_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_9503.R'

cv_mlp_boot_1000_0.95_65769.RData: cv_mlp_boot_1000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_65769.R'

cv_mars_boot_1000_0.95_61920.RData: cv_mars_boot_1000_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_61920.R'

cv_lm_cv_500_0.95_90601.RData: cv_lm_cv_500_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_90601.R'

cv_lm_cv_500_0.9_63935.RData: cv_lm_cv_500_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_63935.R'

cv_lm_cv_1000_0.9_74917.RData: cv_lm_cv_1000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_74917.R'

cv_mlp_boot_1000_0.95_70246.RData: cv_mlp_boot_1000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_70246.R'

cv_lm_boot_500_0.9_65769.RData: cv_lm_boot_500_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_65769.R'

cv_mlp_boot_1000_0.9_93901.RData: cv_mlp_boot_1000_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_93901.R'

cv_lm_cv_500_0.9_65769.RData: cv_lm_cv_500_0.9_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_65769.R'

cv_mlp_boot_500_0.95_63935.RData: cv_mlp_boot_500_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_63935.R'

cv_mars_cv_5000_0.95_82586.RData: cv_mars_cv_5000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_82586.R'

cv_mars_cv_1000_0.95_94074.RData: cv_mars_cv_1000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_94074.R'

cv_mlp_boot_1000_0.9_25462.RData: cv_mlp_boot_1000_0.9_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_25462.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_25462.R'

cv_mlp_boot_500_0.95_90601.RData: cv_mlp_boot_500_0.95_90601.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_90601.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_90601.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_90601.R'

cv_mars_cv_500_0.9_11319.RData: cv_mars_cv_500_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_11319.R'

cv_mlp_cv_1000_0.9_9503.RData: cv_mlp_cv_1000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_9503.R'

cv_mars_boot_5000_0.9_13308.RData: cv_mars_boot_5000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_13308.R'

cv_lm_cv_500_0.95_74917.RData: cv_lm_cv_500_0.95_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.95_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.95_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.95_74917.R'

cv_mlp_cv_5000_0.9_85818.RData: cv_mlp_cv_5000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_85818.R'

cv_mlp_boot_1000_0.95_54686.RData: cv_mlp_boot_1000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.95_54686.R'

cv_mlp_boot_500_0.9_11319.RData: cv_mlp_boot_500_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_11319.R'

cv_lm_cv_1000_0.9_82586.RData: cv_lm_cv_1000_0.9_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_82586.R'

cv_lm_boot_500_0.95_70246.RData: cv_lm_boot_500_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_70246.R'

cv_lm_cv_1000_0.95_11319.RData: cv_lm_cv_1000_0.95_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_11319.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_11319.R'

cv_mlp_boot_500_0.9_88428.RData: cv_mlp_boot_500_0.9_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.9_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.9_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.9_88428.R'

cv_mlp_cv_500_0.95_85818.RData: cv_mlp_cv_500_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_85818.R'

cv_lm_boot_500_0.95_63935.RData: cv_lm_boot_500_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_63935.R'

cv_mlp_cv_5000_0.95_28082.RData: cv_mlp_cv_5000_0.95_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_28082.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_28082.R'

cv_lm_boot_5000_0.95_65769.RData: cv_lm_boot_5000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_65769.R'

cv_mars_boot_500_0.9_85818.RData: cv_mars_boot_500_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_85818.R'

cv_lm_boot_5000_0.9_9503.RData: cv_lm_boot_5000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_9503.R'

cv_lm_cv_5000_0.9_61920.RData: cv_lm_cv_5000_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_61920.R'

cv_mlp_cv_5000_0.95_54686.RData: cv_mlp_cv_5000_0.95_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_54686.R'

cv_mars_cv_1000_0.95_18633.RData: cv_mars_cv_1000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_18633.R'

cv_lm_boot_1000_0.95_70246.RData: cv_lm_boot_1000_0.95_70246.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_70246.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_70246.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_70246.R'

cv_lm_cv_5000_0.95_82586.RData: cv_lm_cv_5000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_82586.R'

cv_mars_cv_500_0.9_94074.RData: cv_mars_cv_500_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_94074.R'

cv_mars_boot_5000_0.95_37035.RData: cv_mars_boot_5000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_37035.R'

cv_lm_boot_1000_0.9_18633.RData: cv_lm_boot_1000_0.9_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_18633.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_18633.R'

cv_lm_cv_5000_0.9_74917.RData: cv_lm_cv_5000_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.9_74917.R'

cv_mars_boot_500_0.9_94074.RData: cv_mars_boot_500_0.9_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_94074.R'

cv_lm_boot_5000_0.95_90992.RData: cv_lm_boot_5000_0.95_90992.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_90992.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_90992.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_90992.R'

cv_lm_cv_5000_0.95_25462.RData: cv_lm_cv_5000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_25462.R'

cv_mlp_boot_5000_0.9_69773.RData: cv_mlp_boot_5000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_5000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_5000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_5000_0.9_69773.R'

cv_lm_cv_500_0.9_13308.RData: cv_lm_cv_500_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_500_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_500_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_500_0.9_13308.R'

cv_lm_boot_5000_0.9_85818.RData: cv_lm_boot_5000_0.9_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_85818.R'

cv_lm_boot_500_0.9_74917.RData: cv_lm_boot_500_0.9_74917.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_74917.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_74917.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_74917.R'

cv_mlp_cv_5000_0.9_54686.RData: cv_mlp_cv_5000_0.9_54686.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_54686.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_54686.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_54686.R'

cv_mlp_cv_500_0.95_18633.RData: cv_mlp_cv_500_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_18633.R'

cv_lm_boot_1000_0.95_33453.RData: cv_lm_boot_1000_0.95_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_33453.R'

cv_mlp_cv_500_0.95_13308.RData: cv_mlp_cv_500_0.95_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_13308.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_13308.R'

cv_mlp_cv_5000_0.9_11319.RData: cv_mlp_cv_5000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_11319.R'

cv_mars_boot_500_0.9_11319.RData: cv_mars_boot_500_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.9_11319.R'

cv_lm_cv_1000_0.95_85818.RData: cv_lm_cv_1000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_85818.R'

cv_lm_boot_1000_0.9_33964.RData: cv_lm_boot_1000_0.9_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_33964.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_33964.R'

cv_lm_boot_1000_0.95_94074.RData: cv_lm_boot_1000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_94074.R'

cv_mars_boot_500_0.95_61920.RData: cv_mars_boot_500_0.95_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_61920.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_61920.R'

cv_lm_boot_500_0.95_52080.RData: cv_lm_boot_500_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.95_52080.R'

cv_mars_cv_1000_0.95_37035.RData: cv_mars_cv_1000_0.95_37035.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_37035.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_37035.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_37035.R'

cv_lm_boot_5000_0.9_69773.RData: cv_lm_boot_5000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_69773.R'

cv_lm_boot_5000_0.9_33453.RData: cv_lm_boot_5000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_33453.R'

cv_mars_boot_1000_0.95_85818.RData: cv_mars_boot_1000_0.95_85818.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_1000_0.95_85818.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_1000_0.95_85818.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_1000_0.95_85818.R'

cv_lm_boot_1000_0.95_52080.RData: cv_lm_boot_1000_0.95_52080.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.95_52080.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.95_52080.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.95_52080.R'

cv_mlp_cv_5000_0.9_63935.RData: cv_mlp_cv_5000_0.9_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.9_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.9_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.9_63935.R'

cv_mars_boot_5000_0.9_9503.RData: cv_mars_boot_5000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.9_9503.R'

cv_mlp_boot_500_0.95_88428.RData: cv_mlp_boot_500_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_500_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_500_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_500_0.95_88428.R'

cv_mlp_cv_1000_0.95_82586.RData: cv_mlp_cv_1000_0.95_82586.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.95_82586.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.95_82586.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.95_82586.R'

cv_mlp_boot_1000_0.9_69773.RData: cv_mlp_boot_1000_0.9_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_boot_1000_0.9_69773.R'
	@$(RCMD) BATCH --vanilla cv_mlp_boot_1000_0.9_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_boot_1000_0.9_69773.R'

cv_lm_boot_500_0.9_61920.RData: cv_lm_boot_500_0.9_61920.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_61920.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_61920.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_61920.R'

cv_lm_boot_5000_0.95_93901.RData: cv_lm_boot_5000_0.95_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.95_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.95_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.95_93901.R'

cv_mars_boot_5000_0.95_33964.RData: cv_mars_boot_5000_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_5000_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_5000_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_5000_0.95_33964.R'

cv_lm_boot_1000_0.9_9503.RData: cv_lm_boot_1000_0.9_9503.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_1000_0.9_9503.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_1000_0.9_9503.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_1000_0.9_9503.R'

cv_lm_cv_1000_0.95_65769.RData: cv_lm_cv_1000_0.95_65769.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_65769.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_65769.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_65769.R'

cv_mlp_cv_1000_0.9_11319.RData: cv_mlp_cv_1000_0.9_11319.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_1000_0.9_11319.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_1000_0.9_11319.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_1000_0.9_11319.R'

cv_mlp_cv_5000_0.95_38345.RData: cv_mlp_cv_5000_0.95_38345.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_38345.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_38345.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_38345.R'

cv_lm_boot_500_0.9_93901.RData: cv_lm_boot_500_0.9_93901.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_500_0.9_93901.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_500_0.9_93901.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_500_0.9_93901.R'

cv_lm_cv_1000_0.95_25462.RData: cv_lm_cv_1000_0.95_25462.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_25462.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_25462.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_25462.R'

cv_mlp_cv_500_0.95_63935.RData: cv_mlp_cv_500_0.95_63935.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_500_0.95_63935.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_500_0.95_63935.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_500_0.95_63935.R'

cv_lm_cv_5000_0.95_88428.RData: cv_lm_cv_5000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_5000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_5000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_5000_0.95_88428.R'

cv_mars_cv_5000_0.95_94074.RData: cv_mars_cv_5000_0.95_94074.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_5000_0.95_94074.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_5000_0.95_94074.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_5000_0.95_94074.R'

cv_mars_cv_1000_0.9_33453.RData: cv_mars_cv_1000_0.9_33453.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.9_33453.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.9_33453.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.9_33453.R'

cv_mars_boot_500_0.95_33964.RData: cv_mars_boot_500_0.95_33964.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_boot_500_0.95_33964.R'
	@$(RCMD) BATCH --vanilla cv_mars_boot_500_0.95_33964.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_boot_500_0.95_33964.R'

cv_mlp_cv_5000_0.95_18633.RData: cv_mlp_cv_5000_0.95_18633.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mlp_cv_5000_0.95_18633.R'
	@$(RCMD) BATCH --vanilla cv_mlp_cv_5000_0.95_18633.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mlp_cv_5000_0.95_18633.R'

cv_mars_cv_1000_0.95_69773.RData: cv_mars_cv_1000_0.95_69773.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_1000_0.95_69773.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_1000_0.95_69773.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_1000_0.95_69773.R'

cv_lm_cv_1000_0.95_88428.RData: cv_lm_cv_1000_0.95_88428.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.95_88428.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.95_88428.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.95_88428.R'

cv_lm_boot_5000_0.9_28082.RData: cv_lm_boot_5000_0.9_28082.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_boot_5000_0.9_28082.R'
	@$(RCMD) BATCH --vanilla cv_lm_boot_5000_0.9_28082.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_boot_5000_0.9_28082.R'

cv_lm_cv_1000_0.9_13308.RData: cv_lm_cv_1000_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_lm_cv_1000_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_lm_cv_1000_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_lm_cv_1000_0.9_13308.R'

cv_mars_cv_500_0.9_13308.RData: cv_mars_cv_500_0.9_13308.R 
	@date '+ %Y-%m-%d %H:%M:%S: + cv_mars_cv_500_0.9_13308.R'
	@$(RCMD) BATCH --vanilla cv_mars_cv_500_0.9_13308.R
	@date '+ %Y-%m-%d %H:%M:%S: - cv_mars_cv_500_0.9_13308.R'
