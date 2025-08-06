generate_quality_message <- function(quality = "purple", sample_type = "cs") {
  st <- ifelse(sample_type == "cs", "clinical sample", "external quality assessment material")
  if (quality == "purple") {
    m1 <- sprintf("Your %s data is of impressive quality! Well done!", st)
    m2 <- sprintf("Excellent work! This set of %s data are perfect for commutability evaluation", st)
    m3 <- sprintf("Alert the scientists: this %s data set is dangerously perfect for commutability evaluation.", st)
    m4 <- sprintf("Perfect %s data. Finally, something in my life that is not a total disappointment.", st)
    m5 <- sprintf("Analysis complete. These %s data are 99.999 percent optimal for commutability evaluation. Rounding up to 100 percent. You're welcome, human.", st);
    m6 <- sprintf("Excellent work. This set of %s data is extremely well-suited for commutability evaluation.", st)
    m7 <- sprintf("The %s data set demonstrates well above necessary characteristics for reliable commutability assessment.", st)
    m8 <- sprintf("These %s data meet the upper criteria for a robust commutability evaluation.", st)
    m9 <- sprintf("This %s data set is more than sufficient for conducting a commutability evaluation.", st)
  }
  else if (quality == "green") {
    m1 <- sprintf("The %s data set meets the minimum quality requirements for commutability evaluation.", st)
    m2 <- sprintf("The uploaded %s data are acceptable for use in commutability analysis.", st)
    m3 <- sprintf("The %s data provide a sufficient basis for proceeding with commutability evaluation.", st)
    m4 <- sprintf("Data quality for the %s set is adequate to support commutability assessment.", st)
    m5 <- sprintf("The submitted %s data are suitable for commutability evaluation under standard criteria.", st)
    m6 <- sprintf("These %s data fulfill the necessary conditions for a valid commutability evaluation.", st)
    m7 <- sprintf("Well, the %s data won’t win any awards—but it’ll do just fine for commutability evaluation. Good hustle!", st)
    m8 <- sprintf("Data ingestion complete. %s data deemed ‘acceptable’. Not impressive, but sufficient. Recommend: minor celebration protocol (e.g., one slow clap).", st)
    m9 <- sprintf("The %s data are acceptable for commutability evaluation. That’s more than I can say about most things these days.", st)
  }
  else if (quality == "yellow") {
    m1 <- sprintf("The %s data set meets the basic requirements for commutability evaluation, though quality is suboptimal.", st)
    m2 <- sprintf("These %s data are acceptable, but significant quality issues were noted.", st)
    m3 <- sprintf("The %s data are marginally sufficient for commutability assessment. Use with caution.", st)
    m4 <- sprintf("Sure, the %s data can be used for commutability evaluation. But at what cost?", st)
    m5 <- sprintf("This %s data set made it over the quality bar. The bar was on the floor, but still.", st)
    m6 <- sprintf("The %s data are acceptable. Like eating cold pasta straight from the fridge — not great, not illegal.", st)
    m7 <- sprintf("These %s data are barely good enough.", st)
    m8 <- sprintf("The %s data are technically usable. But can you trust the results?", st)
    m9 <- sprintf("Quality assessment complete. %s data classified as ‘acceptable’. Enthusiasm level: moderated.", st)
  }
  else if (quality == "red") {
    m1 <- sprintf("The %s data set does not meet the minimum quality requirements for commutability evaluation.", st)
    m2 <- sprintf("Unfortunately, the %s data quality is insufficient for reliable commutability assessment.", st)
    m3 <- sprintf("The %s data cannot be used for commutability evaluation due to critical quality issues.", st)
    m4 <- sprintf("The %s data have bravely attempted to qualify for commutability evaluation... and failed spectacularly.", st)
    m5 <- sprintf("These %s data are so bad, I had to double-check they were not a prank submission.", st)
    m6 <- sprintf("This %s data set is to commutability what a fork is to soup — fundamentally incompatible.", st)
    m7 <- sprintf("These %s data cannot be used. Much like my degree in philosophy.", st)
    m8 <- sprintf("The %s data are unusable. Just like my attempts at maintaining work-life balance.", st)
    m9 <- sprintf("System alert: %s data failed quality screening. Commencing polite shutdown sequence… Beep... boop... no.", st)
  }

  # Collect messages into a vector
  messages <- c(m1, m2, m3, m4, m5, m6, m7, m8, m9)

  # Sample one randomly
  selected_message <- sample(messages, 1)

  return(selected_message)
}
