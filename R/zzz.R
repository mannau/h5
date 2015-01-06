.onLoad <- function(...) {
  # Bind activation of cbind(2) and rbind(2) for S4 classes
  methods:::bind_activation(TRUE)
}

.onUnload <- function(...) {
  # Bind activation of cbind(2) and rbind(2) for S4 classes
  methods:::bind_activation(FALSE)
}
