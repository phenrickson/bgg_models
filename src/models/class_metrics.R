# metrics
class_metrics = metric_set(yardstick::mcc,
                           yardstick::kap,
                           yardstick::precision,
                           yardstick::recall,
                           yardstick::j_index,
                           yardstick::bal_accuracy)

# tune over prob metrics
prob_metrics = metric_set(yardstick::mn_log_loss,
                          yardstick::roc_auc)
