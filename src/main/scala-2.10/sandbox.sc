package sphere
/*
Copyright 2016, Ricardo T. Lemos

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
  limitations under the License.
*/

object example2 {
  // Shows how to get the tuple (distance, azimuth), using a call to distanceAzimuthFinder
  // It is also possible to use a simpler method from the same object, which provides the haversine distance

  val rlat: Double = 20.0
  val rlon: Double = -4.8
  val glat: Double = 20.0
  val glon: Double = -1.0
  val myfmt: String = "radians" //0to360
  val daz= sphereCalc.getDistanceAzimuth(rlat, rlon, glat, glon, myfmt)
  val daz2= sphereCalc.getDistanceAzimuth(rlat, rlon, glat, glon, myfmt,
    earthR = sphereCalc.getLocalEarthRadius(rlat))
  (daz._1 - daz2._1) * 1000.0

  // Tests reliability of distance provided by getDistanceAzimuth
  val distHav = sphereCalc.getHaversineDistance(rlat, rlon, glat, glon)
  val distHav2 = sphereCalc.getHaversineDistance(rlat, rlon, glat, glon,
    earthR = sphereCalc.getLocalEarthRadius(rlat))

  // Shows how to get the List(lon, lat), based on distance and azimuth, using getCoordsFromDistanceAzimuth
  // This tests self consistency
  sphereCalc.getCoordsFromDistanceAzimuth(rlat, rlon, daz._1, daz._2, myfmt)
  sphereCalc.getCoordsFromDistanceAzimuth(rlat, rlon, daz._1, daz._2, myfmt,
    earthR = sphereCalc.getLocalEarthRadius(rlat))
  sphereCalc.getCoordsFromDistanceAzimuth(rlat, rlon, daz2._1, daz2._2, myfmt,
    earthR = sphereCalc.getLocalEarthRadius(rlat))

}

