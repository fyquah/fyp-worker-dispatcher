set -x 

# Ridge models

MODEL="ridge" VERSION="v0-reproduce-relabel" OUT_MODEL="ridge-general" ./scripts/dump_rewards_from_all_experiments.sh general
FEATURE_VERSION="V3" REWARD_MODEL="ridge-general" REWARD_VERSION="v0-reproduce-relabel" ./scripts/merge_feature_and_rewards_from_all_experiments.sh &>out.log
python feature_loader.py ridge-general v3

MODEL="ridge" VERSION="v0-reproduce-relabel" OUT_MODEL="ridge-star" ./scripts/dump_rewards_from_all_experiments.sh star
FEATURE_VERSION="V3" REWARD_MODEL="ridge-star" REWARD_VERSION="v0-reproduce-relabel" ./scripts/merge_feature_and_rewards_from_all_experiments.sh &>out.log
python feature_loader.py ridge-star v3

MODEL="ridge" VERSION="v0-reproduce-relabel" OUT_MODEL="ridge-hand" ./scripts/dump_rewards_from_all_experiments.sh decay-0.400000-ridge-0.050000-benefit-log_speedup_over_mean
FEATURE_VERSION="V3" REWARD_MODEL="ridge-hand" REWARD_VERSION="v0-reproduce-relabel" ./scripts/merge_feature_and_rewards_from_all_experiments.sh &>out.log
python feature_loader.py ridge-hand v3


# lasso models

MODEL="lasso" VERSION="v0-reproduce-relabel" OUT_MODEL="lasso-general" ./scripts/dump_rewards_from_all_experiments.sh general
FEATURE_VERSION="V3" REWARD_MODEL="lasso-general" REWARD_VERSION="v0-reproduce-relabel" ./scripts/merge_feature_and_rewards_from_all_experiments.sh &>out.log
python feature_loader.py lasso-general v3

MODEL="lasso" VERSION="v0-reproduce-relabel" OUT_MODEL="lasso-star" ./scripts/dump_rewards_from_all_experiments.sh star
FEATURE_VERSION="V3" REWARD_MODEL="lasso-star" REWARD_VERSION="v0-reproduce-relabel" ./scripts/merge_feature_and_rewards_from_all_experiments.sh &>out.log
python feature_loader.py lasso-star v3

MODEL="lasso" VERSION="v0-reproduce-relabel" OUT_MODEL="lasso-hand" ./scripts/dump_rewards_from_all_experiments.sh decay-1.000000-benefit-tanh_speedup_over_baseline-lasso-factor-auto
FEATURE_VERSION="V3" REWARD_MODEL="lasso-hand" REWARD_VERSION="v0-reproduce-relabel" ./scripts/merge_feature_and_rewards_from_all_experiments.sh &>out.log
python feature_loader.py lasso-hand v3
