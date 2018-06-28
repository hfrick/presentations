# orginially: https://stackoverflow.com/questions/4974531/writing-musical-notes-to-a-wav-file
# R code adapted from Jim Hester's
# https://github.com/jimhester/presentations/tree/master/2018_02_03-You-can-make-a-package-in-20-minutes#readme

rate <-  44100
multiplier <-  2 * pi / rate
bpm <-  80
default_volume <-  5

notes <- c("A" = 0, "A#" = 1, "Bb" = 1, "B" = 2, "Cb" = 2, "B#" = 3, "C" = 3,
           "C#" = 4, "Db" = 4, "D" = 5, "D#" = 6, "Eb" = 6, "E" = 7, "Fb" = 7,
           "E#" = 8,
           "F" = 8, "F#" = 9, "Gb" = 9, "G" = 10, "G#" = 11, "Ab" = 11)

calc_frequency <- function(note, octave) {
  # 440hz is A above middle C
  440 * 2^((unname(notes[note]) + (octave * 12)) / 12)
}

calc_volume <- function(x, force = F) {
  # x should be between 1 and 10
  if (force){
    if (x<1) x <- 1
    if (x>10) x <- 10
  } else {
    stopifnot(x >= 1, x <= 10)
  }

  x / 10
}

calc_length <- function(rate, length, bpm) {
  seq(1, as.integer(rate * length * 60 / bpm))
}

calc_multiplier <- function(rate) {
  2 * pi / rate
}

#' Title
#'
#' @param note name
#' @param length in beats
#' @param octave from middle C
#' @param volume from 1 to 10
#'
#' @return a note object
#' @export
#'
#' @examples
#' note("C")
note <- function(note, length = 1, octave = 0, volume = default_volume) {
  frequency <- calc_frequency(note, octave)
  volume <- calc_volume(volume)
  length <- calc_length(rate, length, bpm)
  multiplier <- calc_multiplier(rate)
  res <- sin(frequency * multiplier * length) * volume
  structure(res, class = "note")
}


#' Title
#'
#' @param x x
#' @param ... ...
#' @importFrom audio play play.default
#' @export
play <- audio::play

#' @export
print.note <- function(x, ...) {
  audio::play(x, ...)
}

