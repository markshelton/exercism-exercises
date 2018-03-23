module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / seconds_per_year / convert planet
    where   convert Earth = 1
            convert Mercury = 0.2408467 
            convert Venus = 0.61519726
            convert Mars = 1.8808158
            convert Jupiter = 11.862615
            convert Saturn = 29.447498
            convert Uranus = 84.016846
            convert Neptune = 164.79132
            seconds_per_year = 31557600
