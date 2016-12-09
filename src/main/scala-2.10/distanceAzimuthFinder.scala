import breeze.numerics._
import scala.math.{Pi, signum}

/**
  * This object lets you compute distance and azimuth (a.k.a. initial bearing) between 2 points on a sphere's surface.
  * Given a pair of lat/lon coordinates plus an azimuth and a distance, it also computes the endpoint lat/lon. This
  * is useful to check the code for self-consistency. Finally, there's a vanilla haversine distance function, to
  * confirm that the great circle distances provided by the object are correct. Inspired by
  * http://www.movable-type.co.uk/scripts/latlong-vectors.html
  */
object sphereCalc {

  /**
    * Find distance [km] and azimuth [radians/degrees from North] between two pairs of lat/lon cordinates [deg. N and E].
    * Azimuth (a.k.a. initial bearing) is measured as an angle from North to point B, and is positive clockwise.
    * @param latA Latitude of point A [degrees N]
    * @param lonA Longitude of point A [degrees N]
    * @param latB Latitude of point B  [degrees E]
    * @param lonB Longitude of point B  [degrees E]
    * @param azFmt Format of output azimuth. Options: "radians", "-180to180", "0to360", "0to360degminsec"
    * @return Tuple with distance [km] and azimuth [azFmt] from A to B.
    */
  def getDistanceAzimuth(latA: Double, lonA: Double, latB: Double, lonB: Double,
                         azFmt: String = "radians", earthR: Double = 6371.0): (Double, Double) = {
    val aN: List[Double] = convertToNpoint(lonA, latA)
    val bN: List[Double] = convertToNpoint(lonB, latB)
    val distance: Double = distanceNpoints(aN, bN, earthR)
    val azimuth: Double = bearingNpoints(aN, bN, azFmt)
    (distance, azimuth)
  }

  /**
    * Implementation of the vanilla haversine calculator. Checks that getDistanceAzimuth provides correct distances.
    * @param latA Latitude of point A [degrees N]
    * @param lonA Longitude of point A [degrees N]
    * @param latB Latitude of point B  [degrees E]
    * @param lonB Longitude of point B  [degrees E]
    * @param earthR Earth radius [km]
    * @return Distance [km] from A to B
    */
  def getHaversineDistance(latA: Double, lonA: Double, latB: Double, lonB: Double, earthR: Double = 6371.0): Double = {
    val pA: (Double, Double) = (lonA * Pi / 180.0, latA * Pi / 180.0)
    val pB: (Double, Double) = (lonB * Pi / 180.0, latB * Pi / 180.0)
    2.0 * earthR * asin(sqrt(pow(sin(0.5 * (pB._2 - pA._2)), 2) +
      cos(pA._2) * cos(pB._2) * pow(sin(0.5 * (pB._1 - pA._1)), 2)))
  }

  /**
    * Given the lat/lon coordinates of point A plus an azimuth and a distance, computes lat/lon of point B.
    * @param latA Latitude of point A [degrees N]
    * @param lonA Longitude of point A [degrees N]
    * @param distance Distance travelled from point A [km]
    * @param azimuth Bearing from point A, measured as an angle from the true North, positive clockwise
    * @param fmt Format of azimuth. Options: "radians", "-180to180", "0to360", "0to360degminsec"
    * @return List with coordinates (longitude and latitude [degrees]) of point B
    */
  def getCoordsFromDistanceAzimuth(latA: Double, lonA: Double, distance: Double, azimuth: Double,
                                   fmt: String = "radians", earthR: Double = 6371.0): List[Double] = {
    // apologies: I know there's a better way to do this in scala than with a bunch of if/elses...
    val az: Double = if (fmt == "radians") {
      azimuth
    } else if (fmt == "-180to180") {
      azimuth * Pi / 180.0
    } else if (fmt == "0to360") {
      ((azimuth + 180.0) % 360.0 - 180.0) * Pi / 180.0
    } else if (fmt == "0to360degminsec") {
      val deg = floor(azimuth)
      val mn = floor((azimuth - deg) * 100.0)
      val sec = 10000.0 * (azimuth - deg - mn / 100.0)
      val aa = deg + mn / 60 + sec / 3600
      ((aa + 180.0) % 360.0 - 180.0) * Pi / 180.0
    } else {
      0.0
    }
    val dang: Double = distance / earthR
    val northN: List[Double] = List(0.0, 0.0, 1.0)
    val aN: List[Double] = convertToNpoint(lonA, latA)
    val deTmp: List[Double] = crossprodNpoint(northN, aN)
    val de: List[Double] = deTmp.map(x => x / normNpoint(deTmp))
    val dn: List[Double] = crossprodNpoint(aN, de)
    val dd: List[Double] = (dn, de).zipped.map((n, e) => n * cos(az) + e * sin(az))
    val bb: List[Double] = (aN, dd).zipped.map((a, d) => a * cos(dang) + d * sin(dang))
    val br: List[Double] = convertToRadians(bb)
    List(br(0) * 180.0 / Pi, br(1) * 180.0 / Pi)
  }

  /**
    * Provides a latitude-dependent Earth radius, assuming an ellipsoidal Earth with Rmin = 6353 km and Rmax = 6384 km
    * That is, (R * cos(phi) / Rmax) ** 2 + (R * sin(phi) / Rmin) ** 2 = 1, with phi = lat * Pi / 180
    * @param lat Latitude [degrees N]
    * @return Earth radius [km]
    */
  def getLocalEarthRadius(lat: Double) : Double = {
    1.0 / sqrt(pow(cos(lat * Pi / 180.0) / 6384.0, 2) + pow(sin(lat * Pi / 180.0) / 6353.0, 2))
  }

  // ********************************************************************************************************************
  // Private methods
  // ********************************************************************************************************************

  /**
    * Given the lat/lon coordinates of a point, provides the corresponding n-vector (https://en.wikipedia.org/wiki/N-vector)
    * @param lon Longitude [degrees E]
    * @param lat Latitude [degrees N]
    * @return List with n-vector coordinates. Coord0 points to 0°N,0°E, Coord1 points to 0°N,90°E, Coord2 points to 90°N
    */
  def convertToNpoint(lon: Double, lat: Double): List[Double] = {
    val alpha: Double = lat * Pi / 180.0
    val beta: Double = lon * Pi / 180.0
    List(cos(alpha) * cos(beta), cos(alpha) * sin(beta), sin(alpha))
  }

  /**
    * Given an n-vector that defines a point on a sphere, provides the corresponding lat/lon coordinates [radians]
    * @param npoint List with the three coordinates that define the n-vector
    * @return Tuple with longitude and latitude [radians]
    */
  def convertToRadians(npoint: List[Double]): List[Double] = {
    require(npoint.size == 3, "Not an n-vector")
    val lon: Double = atan2(npoint(1), npoint(0))
    val lat: Double = atan2(npoint(2), sqrt(pow(npoint(0), 2) + pow(npoint(1), 2)))
    List(lon, lat)
  }

  /**
    * Compute the cross-product of two n-vectors
    * @param aN List with the three coordinates that define the n-vector A
    * @param bN List with the three coordinates that define the n-vector B
    * @return List with the three coordinates that define the n-vector A * B
    */
  def crossprodNpoint(aN: List[Double], bN: List[Double]): List[Double] = {
    require(aN.size == 3 && bN.size == 3, "Not an n-vector")
    List(aN(1) * bN(2) - aN(2) * bN(1), aN(2) * bN(0) - aN(0) * bN(2), aN(0) * bN(1) - aN(1) * bN(0))
  }

  /**
    * Compute the inner-product of two n-vectors
    * @param aN List with the three coordinates that define the n-vector A
    * @param bN List with the three coordinates that define the n-vector B
    * @return List with the three coordinates that define the n-vector A . B
    */
  def innerprodNpoint(aN: List[Double], bN: List[Double]): Double = {
    require(aN.size == 3 && bN.size == 3, "Not an n-vector")
    aN(0) * bN(0) + aN(1) * bN(1) + aN(2) * bN(2)
  }

  /**
    * Compute the distance between two n-points
    * @param aN List with the three coordinates that define the n-vector A
    * @param bN List with the three coordinates that define the n-vector B
    * @param earthR Earth radius [km]
    * @return Distance [km] between A and B
    */
  def distanceNpoints(aN: List[Double], bN: List[Double], earthR: Double): Double = {
    require(aN.size == 3 && bN.size == 3, "Not an n-vector")
    earthR * atan2(normNpoint(crossprodNpoint(aN, bN)), innerprodNpoint(aN, bN))
  }

  /**
    * Find the norm of an n-vector
    * @param v List with the three coordinates that define the n-vector v
    * @return Norm of v
    */
  def normNpoint(v: List[Double]): Double = {
    require(v.size == 3, "Not an n-vector")
    sqrt(innerprodNpoint(v, v))
  }

  /**
    * Given two n-vectors, A and B, find the initial bearing from A to B
    * @param aN List with the three coordinates that define the n-vector A
    * @param bN List with the three coordinates that define the n-vector B
    * @param fmt Format of output azimuth. Options: "radians", "-180to180", "0to360", "0to360degminsec"
    * @return Azimuth, or initial bearing, from A to B, in the units desired
    */
  def bearingNpoints(aN: List[Double], bN: List[Double], fmt: String = "0to360"): Double = {
    val northN: List[Double] = List(0.0, 0.0, 1.0)
    val c1: List[Double] = crossprodNpoint(aN, bN)
    val c2: List[Double] = crossprodNpoint(aN, northN)
    val c1c2 = crossprodNpoint(c1, c2)
    val bearing: Double = atan2(normNpoint(c1c2) * signum(innerprodNpoint(c1c2, aN)), innerprodNpoint(c1, c2))
    // again, apologies for the if/elses
    if (fmt == "radians") {
      bearing
    } else if (fmt == "-180to180") {
      180.0 * bearing / Pi
    } else if (fmt == "0to360") {
      (180.0 * bearing / Pi + 360.0) % 360.0
    } else if (fmt == "0to360degminsec") {
      val ang: Double = (180.0 * bearing / Pi + 360.0) % 360.0
      val deg: Double = floor(ang).toDouble
      val mn: Double = floor((ang - deg) * 60.0).toDouble
      val sc: Double = round(((ang - deg) * 60 - mn) * 60.0).toDouble
      deg + mn / 100.0 + sc / 10000.0
    } else {
      -999.99
    }
  }
}

object example1 {
  // Shows how to get the tuple (distance, azimuth), using a call to sphereCalc
  // It is also possible to use a simpler method from the same object, which provides the haversine distance

  val rlat: Double = 23.0
  val rlon: Double = -5.0
  val glat: Double = 25.0
  val glon: Double = -3.0
  val myEarthR: Double = sphereCalc.getLocalEarthRadius(rlat)
  val myfmt: String = "radians" //0to360
  val daz= sphereCalc.getDistanceAzimuth(rlat, rlon, glat, glon, myfmt, earthR = myEarthR)

  // Tests reliability of distance provided by getDistanceAzimuth
  val distHav  = sphereCalc.getHaversineDistance(rlat, rlon, glat, glon, earthR = myEarthR)

  // Shows how to get the List(lon, lat), based on distance and azimuth, using getCoordsFromDistanceAzimuth
  // This tests self consistency
  sphereCalc.getCoordsFromDistanceAzimuth(rlat, rlon, daz._1, daz._2, myfmt, earthR = myEarthR)

}