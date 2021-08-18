
# evaluate within
{
  test_cohort1 <- setdiff(config("cohort")[[src[1]]], train_cohort)
  test <- load_data(src[1], cfg_sparse, times - 24L, times,
                    cohort = test_cohort1)
  
  int1 <- dose_otp(test, times, dose_sparse, sofa[[src[1]]], test_cohort1,
                   src[1])
}

# evaluate outside 1st
{
  test2 <- load_data(src[2], cfg_sparse, times - 24L, times,
                     cohort = config("cohort")[[src[2]]])
  ext1 <- dose_otp(test2, times, dose_sparse, sofa[[src[2]]],
                   config("cohort")[[src[2]]], src[2])
}

# evaluate outside 2nd
{
  test3 <- load_data(src[3], cfg_sparse, times - 24L, times,
                     cohort = config("cohort")[[src[3]]])
  ext2 <- dose_otp(test3, times, dose_sparse, sofa[[src[3]]],
                   config("cohort")[[src[3]]], src[3])
}

fig1 <- otp_fig(rbind(int1, ext1, ext2))
ggsave(file.path(root, "figures", "Figure1.tiff"), fig1,
       width = 12, height = 7)