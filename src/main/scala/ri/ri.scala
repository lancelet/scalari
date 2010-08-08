package ri

import scala.util.DynamicVariable
import scala.collection.{Map, Seq}

case class Matrix(
  e11: Double, e12: Double, e13: Double, e14: Double,
  e21: Double, e22: Double, e23: Double, e24: Double,
  e31: Double, e32: Double, e33: Double, e34: Double,
  e41: Double, e42: Double, e43: Double, e44: Double
)

case class BoundBox(
  xmin: Double, xmax: Double, ymin: Double, ymax: Double, zmin: Double, zmax: Double
)

trait Basis {
  val name: Option[String]
  val matrix: Option[Matrix]
}
class NamedBasis(nme: String) extends Basis {
  val name: Option[String] = Some(nme)
  val matrix: Option[Matrix] = None
}
case class MatrixBasis(mtrx: Matrix) extends Basis {
  val name: Option[String] = None
  val matrix: Option[Matrix] = Some(mtrx)
}
object BezierBasis extends NamedBasis("bezier")
object BSplineBasis extends NamedBasis("b-spline")
object CatmullRomBasis extends NamedBasis("catmull-rom")
object HermiteBasis extends NamedBasis("hermite")
object PowerBasis extends NamedBasis("power")

class RiException(msg: String) extends Exception(msg)
class RiIllegalContextException(msg: String) extends RiException(msg)
class RiNotImplementedException(msg: String) extends RiException(msg)
class RiBadParamList(msg: String) extends RiException(msg)
class RiRendererException(msg: String) extends RiException(msg)

case class FilterFunc(name: String)
object BoxFilter extends FilterFunc("box")
object TriangleFilter extends FilterFunc("triangle")
object CatmullRomFilter extends FilterFunc("catmull-rom")
object GaussianFilter extends FilterFunc("gaussian")
object SincFilter extends FilterFunc("sinc")
object MitchellFilter extends FilterFunc("mitchell")

case class ProjectionType(name: String)
object PerspectiveProjection extends ProjectionType("perspective")
object OrthographicProjection extends ProjectionType("orthographic")

case class QuantizeType(name: String)
object QuantizeRGBA extends QuantizeType("rgba")
object QuantizeZ extends QuantizeType("z")

case class DisplayType(name: String)
object DisplayFrameBuffer extends DisplayType("framebuffer")
object DisplayFile extends DisplayType("file")
object DisplayShadow extends DisplayType("shadow")

case class DisplayMode(name: String)
object DisplayRGB extends DisplayMode("rgb")
object DisplayRGBA extends DisplayMode("rgba")
object DisplayZ extends DisplayMode("z")

case class ShadingInterpolationType(name: String)
object ConstantInterp extends ShadingInterpolationType("constant")
object SmoothInterp extends ShadingInterpolationType("smooth")

sealed case class OrientationType(name: String)
object LeftHanded extends OrientationType("lh")
object RightHanded extends OrientationType("rh")

sealed case class PatchType(name: String)
object Bilinear extends PatchType("bilinear")
object Bicubic extends PatchType("bicubic")

sealed case class PatchWrap(name: String)
object Periodic extends PatchWrap("periodic")
object NonPeriodic extends PatchWrap("nonperiodic")

sealed case class SolidOp(name: String)
object Primitive extends SolidOp("primitive")
object Intersection extends SolidOp("intersection")
object Union extends SolidOp("union")
object Difference extends SolidOp("difference")

trait ObjectHandle
object EmptyObjectHandle extends ObjectHandle

trait LightHandle
object EmptyLightHandle extends LightHandle

trait Context {

  import Context._

  private def throwContextException(): Unit = throw new RiIllegalContextException(
    "Invalid context for that Ri method (or it is not yet implemented)"
  )

  def close(): Unit

  // Retrieve different contexts
  def getFrameContext(): Context = { throwContextException(); this }
  def getWorldContext(): Context = { throwContextException(); this }
  def getAttributeContext(): Context = { throwContextException(); this }
  def getTransformContext(): Context = { throwContextException(); this }
  def getSolidContext(): Context = { throwContextException(); this }
  def getObjectContext(): Context = { throwContextException(); this }
  def getMotionContext(): Context = { throwContextException(); this }
  
  // Start / end blocks
  def frameBegin(frame: Int): Unit = throwContextException()
  def frameEnd(): Unit = throwContextException()
  def worldBegin(): Unit = throwContextException()
  def worldEnd(): Unit = throwContextException()
  def attributeBegin(): Unit = throwContextException()
  def attributeEnd(): Unit = throwContextException()
  def transformBegin(): Unit = throwContextException()
  def transformEnd(): Unit = throwContextException()
  def solidBegin(op: SolidOp): Unit = throwContextException()
  def solidEnd(): Unit = throwContextException()
  def objectBegin(): Unit = throwContextException()
  def objectEnd(): Unit = throwContextException()
  def getContext(): Context = this
  def getObjectHandle(): ObjectHandle = { throwContextException(); EmptyObjectHandle }
  def motionBegin(times: Seq[Double]): Unit = throwContextException()
  def motionEnd(): Unit = throwContextException()
  
  // Options
  def declare(name: String, declaration: String): Unit = throwContextException()
  def format(xres: Int, yres: Int, aspect: Double): Unit = throwContextException()
  def frameAspectRatio(aspect: Double): Unit = throwContextException()
  def screenWindow(left: Double, right: Double, bottom: Double, top: Double): Unit = throwContextException()
  def cropWindow(xmin: Double, xmax: Double, ymin: Double, ymax: Double): Unit = throwContextException()
  def projection(pType: ProjectionType, params: PMap): Unit = throwContextException()
  def clipping(near: Double, far: Double): Unit = throwContextException()
  def depthOfField(fStop: Double, focalLength: Double, focalDistance: Double): Unit = throwContextException()
  def shutter(min: Double, max: Double): Unit = throwContextException()
  def pixelVariance(variation: Double): Unit = throwContextException()
  def pixelSamples(xsamples: Double, ysamples: Double): Unit = throwContextException()
  def pixelFilter(filterFunc: FilterFunc, xwidth: Double, ywidth: Double): Unit = throwContextException()
  def exposure(gain: Double, gamma: Double): Unit = throwContextException()
  def imager(name: String, params: PMap): Unit = throwContextException()
  def quantize(qType: QuantizeType, one: Int, min: Int, max: Int, ditherAmplitude: Double): Unit = 
    throwContextException()
  def display(name: String, dType: DisplayType, dMode: DisplayMode, params: PMap): Unit = throwContextException()
  def hider(name: String, params: PMap): Unit = throwContextException()
  def colorSamples(nRGB: Seq[Double], RGBn: Seq[Double]): Unit = throwContextException()
  def relativeDetail(relativeDetail: Double): Unit = throwContextException()
  def option(name: String, params: PMap): Unit = throwContextException()

  // Lights
  def lightSource(shaderName: String, params: PMap): LightHandle = { throwContextException(); EmptyLightHandle }
  def areaLightSource(shaderName: String, params: PMap): LightHandle = { throwContextException(); EmptyLightHandle }

  // Attributes
  def attribute(name: String, params: PMap): Unit = throwContextException()
  def color(c: Seq[Double]): Unit = throwContextException()
  def opacity(c: Seq[Double]): Unit = throwContextException()
  def surface(shaderName: String, params: PMap): Unit = throwContextException()
  def atmosphere(shaderName: String, params: PMap): Unit = throwContextException()
  def interior(shaderName: String, params: PMap): Unit = throwContextException()
  def exterior(shaderName: String, params: PMap): Unit = throwContextException()
  def illuminate(handle: LightHandle, onOff: Boolean): Unit = throwContextException()
  def displacement(shaderName: String, params: PMap): Unit = throwContextException()
  def textureCoordinates(s1: Double, t1: Double, s2: Double, t2: Double, 
    s3: Double, t3: Double, s4: Double, t4: Double): Unit = throwContextException()
  def shadingRate(size: Double): Unit = throwContextException()
  def shadingInterpolation(iType: ShadingInterpolationType): Unit = throwContextException()
  def matte(onOff: Boolean): Unit = throwContextException()
  def bound(bound: BoundBox): Unit = throwContextException()
  def detail(bound: BoundBox): Unit = throwContextException()
  def detailRange(minVisible: Double, lowerTransition: Double, upperTransition: Double, maxVisible: Double): Unit =
    throwContextException()
  def geometricApproximation(aType: String, value: Double): Unit = throwContextException()
  def orientation(orientation: OrientationType): Unit = throwContextException()
  def reverseOrientation(): Unit = throwContextException()
  def sides(sides: Int): Unit = throwContextException()
  def basis(uBasis: Basis, uStep: Int, vBasis: Basis, vStep: Int): Unit = throwContextException()
  def trimCurve(nCurves: Seq[Int], order: Seq[Int], knot: Seq[Double], min: Double, max: Double,
    n: Seq[Int], u: Seq[Double], v: Seq[Double], w: Seq[Double]): Unit = throwContextException()

  // Transformations
  def identity(): Unit = throwContextException()
  def transform(transform: Matrix): Unit = throwContextException()
  def concatTransform(transform: Matrix): Unit = throwContextException()
  def perspective(fov: Double): Unit = throwContextException()
  def translate(dx: Double, dy: Double, dz: Double): Unit = throwContextException()
  def rotate(angle: Double, dx: Double, dy: Double, dz: Double): Unit = throwContextException()
  def scale(sx: Double, sy: Double, sz: Double): Unit = throwContextException()
  def skew(angle: Double, dx1: Double, dy1: Double, dz1: Double, dx2: Double, dy2: Double, dz2: Double): Unit =
    throwContextException()
  def deformation(): Unit = throw new RiNotImplementedException("RiDeformation() is not implemented.")

  // Polygons
  def polygon(params: PMap): Unit = throwContextException()
  def generalPolygon(nVertices: Seq[Int], params: PMap): Unit = throwContextException()
  def pointsPolygons(nVertices: Seq[Int], vertices: Seq[Int], params: PMap): Unit = throwContextException()
  def pointsGeneralPolygons(nLoops: Seq[Int], nVertices: Seq[Int], vertices: Seq[Int], params: PMap): Unit = 
    throwContextException()

  // Patches
  def patch(pType: PatchType, params: PMap): Unit = throwContextException()
  def patchMesh(pType: PatchType, nu: Int, uWrap: PatchWrap, nv: Int, vWrap: PatchWrap, params: PMap): Unit =
    throwContextException()
  def nuPatch(nu: Int, uOrder: Int, uKnot: Seq[Double], uMin: Double, uMax: Double,
    nv: Int, vOrder: Int, vKnot: Seq[Double], vMin: Double, vMax: Double, params: PMap): Unit = throwContextException()

  // Quadrics
  def sphere(radius: Double, zmin: Double, zmax: Double, thetaMax: Double, params: PMap): Unit = 
    throwContextException()
  def cone(height: Double, radius: Double, thetaMax: Double, params: PMap): Unit = throwContextException()
  def cylinder(radius: Double, zmin: Double, zmax: Double, thetaMax: Double, params: PMap): Unit =
    throwContextException()
  def hyperboloid(point1: Seq[Double], point2: Seq[Double], thetaMax: Double, params: PMap): Unit =
    throwContextException()
  def paraboloid(rmax: Double, zmin: Double, zmax: Double, thetaMax: Double, params: PMap): Unit =
    throwContextException()
  def disk(height: Double, radius: Double, thetaMax: Double, params: PMap): Unit = throwContextException()
  def torus(majorRadius: Double, minorRadius: Double, phiMin: Double, phiMax: Double, thetaMax: Double, 
    params: PMap): Unit = throwContextException()

  // General objects
  def objectInstance(handle: ObjectHandle): Unit = throwContextException()
  def procedural(name: String, args: Seq[Any], bound: BoundBox): Unit = throwContextException()
  def geometry(name: String, params: PMap): Unit = throwContextException()

  // Map-making
  def makeTexture(pictureName: String, textureName: String, sWrap: String, tWrap: String, filter: FilterFunc,
    sWidth: Double, tWidth: Double, params: PMap): Unit = throwContextException()
  def makeLatLongEnvironment(pictureName: String, textureName: String, filter: FilterFunc, 
    sWidth: Double, tWidth: Double, params: PMap): Unit = throwContextException()
  def makeCubeFaceEnvironment(px: String, nx: String, py: String, ny: String, pz: String, nz: String,
    textureName: String, fov: Double, filter: FilterFunc, sWidth: Double, tWidth: Double, params: PMap): Unit =
    throwContextException()
  def makeShadow(pictureName: String, textureName: String, params: PMap): Unit = throwContextException()
}
object Context {
  type PMap = Map[String, Seq[Any]]
}


class Ri {
  import Context._
  private val ctx = new DynamicVariable[Option[Context]](None)
  
  def Begin(fileOrRenderer: String)(f: => Unit): Unit = {
    val writer: RibWriter = fileOrRenderer match {
      case "aqsis" => new AqsisRibWriter()
      case "pixie" => new PixieRibWriter()
      case x => FileRibWriter(x)
    }
    val context: Context = new RibTopContext(writer)
    Begin(context)(f)
  }
  def Begin(context: Context = new RibTopContext(new DefaultRibWriter))(f: => Unit): Unit = 
    ctx.withValue(Some(context)) { 
      f 
      context.close()
    }
  def Resume(context: Context)(f: => Unit): Unit = ctx.withValue(Some(context)) { f }
  def getContext(): Context = {
    if (ctx.value.isDefined) {
      ctx.value.get.getContext()
    } else null
  }
  def FrameBlock(frame: Int)(f: => Unit): Unit = ctx.value foreach { context =>
    context.frameBegin(frame)
    ctx.withValue(Some(context.getFrameContext())) { f }
    context.frameEnd()
  }
  def WorldBlock(f: => Unit): Unit = ctx.value foreach { context =>
    context.worldBegin()
    ctx.withValue(Some(context.getWorldContext())) { f }
    context.worldEnd()
  }
  def AttributeBlock(f: => Unit): Unit = ctx.value foreach { context =>
    context.attributeBegin()
    ctx.withValue(Some(context.getAttributeContext())) { f }
    context.attributeEnd()
  }
  def TransformBlock(f: => Unit): Unit = ctx.value foreach { context =>
    context.transformBegin()
    ctx.withValue(Some(context.getTransformContext())) { f }
    context.transformEnd()
  }
  def SolidBlock(op: SolidOp)(f: => Unit): Unit = ctx.value foreach { context =>
    context.solidBegin(op)
    ctx.withValue(Some(context.getSolidContext())) { f }
    context.solidEnd()
  }
  def ObjectBlock(f: => Unit): ObjectHandle = {
    if (ctx.value.isDefined) {
      val context = ctx.value.get
      context.objectBegin()
      val objectContext = context.getObjectContext()
      ctx.withValue(Some(objectContext)) { f }
      context.objectEnd()
      return objectContext.getObjectHandle()      
    } else {
      EmptyObjectHandle
    }
  }
  def MotionBlock(times: Seq[Double])(f: => Unit): Unit = ctx.value foreach { context =>
    context.motionBegin(times)
    ctx.withValue(Some(context.getMotionContext())) { f }
    context.motionEnd()
  }  
  
  /** Extracts a parameter map (PMap) from a varargs array. */
  def extractParams(params: Any*): PMap = {
    val pairs = params.grouped(2)   // group each pair of name and sequence
    val pMap: PMap = (for (p <- pairs) yield {
      val name: String = p.head match {
        case s: String => s
        case _ => throw new RiBadParamList("A String was expected in a parameter list, but was not found.") 
      }
      val item: Seq[_] = p.last match {
        case i: Seq[_] => i
        case b: String => Seq("\"%s\"" format(b))
        case j: Any => Seq(j)
      }
      (name -> item)
    }).toMap
    pMap
  }
  
  // Options
  def Declare(name: String, declaration: String): Unit = ctx.value foreach(_.declare(name, declaration))
  def Format(xres: Int, yres: Int, aspect: Double): Unit = ctx.value foreach(_.format(xres, yres, aspect))
  def FrameAspectRatio(aspect: Double): Unit = ctx.value foreach(_.frameAspectRatio(aspect))
  def ScreenWindow(left: Double, right: Double, bottom: Double, top: Double): Unit =
    ctx.value foreach (_.screenWindow(left, right, bottom, top))
  def CropWindow(xmin: Double, xmax: Double, ymin: Double, ymax: Double): Unit =
    ctx.value foreach (_.cropWindow(xmin, xmax, ymin, ymax))
  def Projection(pType: ProjectionType, params: PMap): Unit = ctx.value foreach (_.projection(pType, params))
  def Projection(pType: ProjectionType, params: Any*): Unit = 
    ctx.value foreach (_.projection(pType, extractParams(params: _*)))
  def Clipping(near: Double, far: Double): Unit = ctx.value foreach (_.clipping(near, far))
  def DepthOfField(fStop: Double, focalLength: Double, focalDistance: Double): Unit =
    ctx.value foreach (_.depthOfField(fStop, focalLength, focalDistance))
  def Shutter(min: Double, max: Double): Unit = ctx.value foreach (_.shutter(min, max))
  def PixelVariance(variation: Double): Unit = ctx.value foreach (_.pixelVariance(variation))
  def PixelSamples(xsamples: Double, ysamples: Double): Unit = ctx.value foreach (_.pixelSamples(xsamples, ysamples))
  def PixelFilter(filterFunc: FilterFunc, xwidth: Double, ywidth: Double): Unit =
    ctx.value foreach (_.pixelFilter(filterFunc, xwidth, ywidth))
  def Exposure(gain: Double, gamma: Double): Unit = ctx.value foreach (_.exposure(gain, gamma))
  def Imager(name: String, params: PMap): Unit = ctx.value foreach (_.imager(name, params))
  def Imager(name: String, params: Any*): Unit = ctx.value foreach (_.imager(name, extractParams(params: _*)))
  def Quantize(qType: QuantizeType, one: Int, min: Int, max: Int, ditherAmplitude: Double): Unit =
    ctx.value foreach (_.quantize(qType, one, min, max, ditherAmplitude))
  def Display(name: String, dType: DisplayType, dMode: DisplayMode, params: PMap): Unit =
    ctx.value foreach (_.display(name, dType, dMode, params))
  def Display(name: String, dType: DisplayType, dMode: DisplayMode, params: Any*): Unit =
    ctx.value foreach (_.display(name, dType, dMode, extractParams(params: _*)))
  def Hider(name: String, params: PMap): Unit = ctx.value foreach (_.hider(name, params))
  def Hider(name: String, params: Any*): Unit = ctx.value foreach (_.hider(name, extractParams(params: _*)))
  def ColorSamples(nRGB: Seq[Double], RGBn: Seq[Double]): Unit = ctx.value foreach (_.colorSamples(nRGB, RGBn))
  def RelativeDetail(relativeDetail: Double): Unit = ctx.value foreach (_.relativeDetail(relativeDetail))
  def Option(name: String, params: PMap): Unit = ctx.value foreach (_.option(name, params))
  def Option(name: String, params: Any*): Unit = ctx.value foreach (_.option(name, extractParams(params: _*)))
  
  // Lights
  def LightSource(shaderName: String, params: PMap): LightHandle = if (ctx.value.isDefined) {
    val context = ctx.value.get
    context.lightSource(shaderName, params)
  } else { EmptyLightHandle }
  def LightSource(shaderName: String, params: Any*): LightHandle = LightSource(shaderName, extractParams(params: _*))
  def AreaLightSource(shaderName: String, params: PMap): LightHandle = if (ctx.value.isDefined) {
    val context = ctx.value.get
    context.areaLightSource(shaderName, params)
  } else { EmptyLightHandle }
  def AreaLightSource(shaderName: String, params: Any*): LightHandle = 
    AreaLightSource(shaderName, extractParams(params: _*))
  
  // Attributes
  def Attribute(name: String, params: PMap): Unit = ctx.value foreach (_.attribute(name, params))
  def Attribute(name: String, params: Any*): Unit = ctx.value foreach (_.attribute(name, extractParams(params: _*)))
  def Color(c: Seq[Double]): Unit = ctx.value foreach (_.color(c))
  def Opacity(c: Seq[Double]): Unit = ctx.value foreach (_.opacity(c))
  def Surface(shaderName: String, params: PMap): Unit = ctx.value foreach (_.surface(shaderName, params))
  def Surface(shaderName: String, params: Any*): Unit = 
    ctx.value foreach (_.surface(shaderName, extractParams(params: _*)))
  def Atmosphere(shaderName: String, params: PMap): Unit = ctx.value foreach (_.atmosphere(shaderName, params))
  def Atmosphere(shaderName: String, params: Any*): Unit = 
    ctx.value foreach (_.atmosphere(shaderName, extractParams(params: _*)))
  def Interior(shaderName: String, params: PMap): Unit = ctx.value foreach (_.interior(shaderName, params))
  def Interior(shaderName: String, params: Any*): Unit = 
    ctx.value foreach (_.interior(shaderName, extractParams(params: _*)))
  def Exterior(shaderName: String, params: PMap): Unit = ctx.value foreach (_.exterior(shaderName, params))
  def Exterior(shaderName: String, params: Any*): Unit = 
    ctx.value foreach (_.exterior(shaderName, extractParams(params: _*)))  
  def Illuminate(handle: LightHandle, onOff: Boolean): Unit = ctx.value foreach (_.illuminate(handle, onOff))
  def Displacement(shaderName: String, params: PMap): Unit = ctx.value foreach (_.displacement(shaderName, params))
  def Displacement(shaderName: String, params: Any*): Unit = 
    ctx.value foreach (_.displacement(shaderName, extractParams(params: _*)))
  def TextureCoordinates(s1: Double, t1: Double, s2: Double, t2: Double, 
    s3: Double, t3: Double, s4: Double, t4: Double): Unit = 
    ctx.value foreach (_.textureCoordinates(s1, t1, s2, t2, s3, t3, s4, t4))
  def ShadingRate(size: Double): Unit = ctx.value foreach (_.shadingRate(size))
  def ShadingInterpolation(iType: ShadingInterpolationType): Unit = ctx.value foreach (_.shadingInterpolation(iType))
  def Matte(onOff: Boolean): Unit = ctx.value foreach (_.matte(onOff))
  def Bound(bound: BoundBox): Unit = ctx.value foreach (_.bound(bound))
  def Detail(bound: BoundBox): Unit = ctx.value foreach (_.detail(bound))
  def DetailRange(minVisible: Double, lowerTransition: Double, upperTransition: Double, maxVisible: Double): Unit =
    ctx.value foreach (_.detailRange(minVisible, lowerTransition, upperTransition, maxVisible))
  def GeometricApproximation(aType: String, value: Double): Unit =
    ctx.value foreach (_.geometricApproximation(aType, value))
  def Orientation(orientation: OrientationType): Unit = ctx.value foreach (_.orientation(orientation))
  def ReverseOrientation(): Unit = ctx.value foreach (_.reverseOrientation())
  def Sides(sides: Int): Unit = ctx.value foreach (_.sides(sides))
  def Basis(uBasis: Basis, uStep: Int, vBasis: Basis, vStep: Int): Unit =
    ctx.value foreach (_.basis(uBasis, uStep, vBasis, vStep))
  def TrimCurve(nCurves: Seq[Int], order: Seq[Int], knot: Seq[Double], min: Double, max: Double,
    n: Seq[Int], u: Seq[Double], v: Seq[Double], w: Seq[Double]): Unit =
    ctx.value foreach (_.trimCurve(nCurves, order, knot, min, max, n, u, v, w))
    
  // Transformations
  def Identity(): Unit = ctx.value foreach (_.identity())
  def Transform(transform: Matrix): Unit = ctx.value foreach (_.transform(transform))
  def ConcatTransform(transform: Matrix): Unit = ctx.value foreach (_.concatTransform(transform))
  def Perspective(fov: Double): Unit = ctx.value foreach (_.perspective(fov))
  def Translate(dx: Double, dy: Double, dz: Double): Unit = ctx.value foreach (_.translate(dx, dy, dz))
  def Rotate(angle: Double, dx: Double, dy: Double, dz: Double): Unit = ctx.value foreach (_.rotate(angle, dx, dy, dz))
  def Scale(sx: Double, sy: Double, sz: Double): Unit = ctx.value foreach (_.scale(sx, sy, sz))
  def Skew(angle: Double, dx1: Double, dy1: Double, dz1: Double, dx2: Double, dy2: Double, dz2: Double): Unit =
    ctx.value foreach (_.skew(angle, dx1, dy1, dz1, dx2, dy2, dz2))
  def Deformation(): Unit = ctx.value foreach (_.deformation())

  // Polygons
  def Polygon(params: PMap): Unit = ctx.value foreach (_.polygon(params))
  def Polygon(params: Any*): Unit = ctx.value foreach (_.polygon(extractParams(params: _*)))
  def GeneralPolygon(nVertices: Seq[Int], params: PMap): Unit =
    ctx.value foreach (_.generalPolygon(nVertices, params))
  def GeneralPolygon(nVertices: Seq[Int], params: Any*): Unit =
    ctx.value foreach (_.generalPolygon(nVertices, extractParams(params: _*)))
  def PointsPolygons(nVertices: Seq[Int], vertices: Seq[Int], params: PMap): Unit =
    ctx.value foreach (_.pointsPolygons(nVertices, vertices, params))
  def PointsPolygons(nVertices: Seq[Int], vertices: Seq[Int], params: Any*): Unit =
    ctx.value foreach (_.pointsPolygons(nVertices, vertices, extractParams(params: _*)))
  def PointsGeneralPolygons(nLoops: Seq[Int], nVertices: Seq[Int], vertices: Seq[Int], params: PMap): Unit =
    ctx.value foreach (_.pointsGeneralPolygons(nLoops, nVertices, vertices, params))
  def PointsGeneralPolygons(nLoops: Seq[Int], nVertices: Seq[Int], vertices: Seq[Int], params: Any*): Unit =
    ctx.value foreach (_.pointsGeneralPolygons(nLoops, nVertices, vertices, extractParams(params: _*)))

  // Patches
  def Patch(pType: PatchType, params: PMap): Unit = ctx.value foreach (_.patch(pType, params))
  def Patch(pType: PatchType, params: Any*): Unit = ctx.value foreach (_.patch(pType, extractParams(params: _*)))
  def PatchMesh(pType: PatchType, nu: Int, uWrap: PatchWrap, nv: Int, vWrap: PatchWrap, params: PMap): Unit =
    ctx.value foreach (_.patchMesh(pType, nu, uWrap, nv, vWrap, params))
  def PatchMesh(pType: PatchType, nu: Int, uWrap: PatchWrap, nv: Int, vWrap: PatchWrap, params: Any*): Unit =
    ctx.value foreach (_.patchMesh(pType, nu, uWrap, nv, vWrap, extractParams(params: _*)))
  def NuPatch(nu: Int, uOrder: Int, uKnot: Seq[Double], uMin: Double, uMax: Double,
    nv: Int, vOrder: Int, vKnot: Seq[Double], vMin: Double, vMax: Double, params: PMap): Unit =
    ctx.value foreach (_.nuPatch(nu, uOrder, uKnot, uMin, uMax, nv, vOrder, vKnot, vMin, vMax, params))
  def NuPatch(nu: Int, uOrder: Int, uKnot: Seq[Double], uMin: Double, uMax: Double,
    nv: Int, vOrder: Int, vKnot: Seq[Double], vMin: Double, vMax: Double, params: Any*): Unit =
    ctx.value foreach (_.nuPatch(nu, uOrder, uKnot, uMin, uMax, nv, vOrder, vKnot, vMin, vMax, 
      extractParams(params: _*)))

  // Quadrics
  def Sphere(radius: Double, zmin: Double, zmax: Double, thetaMax: Double, params: PMap): Unit =
    ctx.value foreach (_.sphere(radius, zmin, zmax, thetaMax, params))
  def Sphere(radius: Double, zmin: Double, zmax: Double, thetaMax: Double, params: Any*): Unit =
    ctx.value foreach (_.sphere(radius, zmin, zmax, thetaMax, extractParams(params: _*)))
  def Cone(height: Double, radius: Double, thetaMax: Double, params: PMap): Unit =
    ctx.value foreach (_.cone(height, radius, thetaMax, params))
  def Cone(height: Double, radius: Double, thetaMax: Double, params: Any*): Unit =
    ctx.value foreach (_.cone(height, radius, thetaMax, extractParams(params: _*)))
  def Cylinder(radius: Double, zmin: Double, zmax: Double, thetaMax: Double, params: PMap): Unit =
    ctx.value foreach (_.cylinder(radius, zmin, zmax, thetaMax, params))
  def Cylinder(radius: Double, zmin: Double, zmax: Double, thetaMax: Double, params: Any*): Unit =
    ctx.value foreach (_.cylinder(radius, zmin, zmax, thetaMax, extractParams(params: _*)))
  def Hyperboloid(point1: Seq[Double], point2: Seq[Double], thetaMax: Double, params: PMap): Unit =
    ctx.value foreach (_.hyperboloid(point1, point2, thetaMax, params))
  def Hyperboloid(point1: Seq[Double], point2: Seq[Double], thetaMax: Double, params: Any*): Unit =
    ctx.value foreach (_.hyperboloid(point1, point2, thetaMax, extractParams(params: _*)))
  def Paraboloid(rmax: Double, zmin: Double, zmax: Double, thetaMax: Double, params: PMap): Unit =
    ctx.value foreach (_.paraboloid(rmax, zmin, zmax, thetaMax, params))
  def Paraboloid(rmax: Double, zmin: Double, zmax: Double, thetaMax: Double, params: Any*): Unit =
    ctx.value foreach (_.paraboloid(rmax, zmin, zmax, thetaMax, extractParams(params: _*)))
  def Disk(height: Double, radius: Double, thetaMax: Double, params: PMap): Unit =
    ctx.value foreach (_.disk(height, radius, thetaMax, params))
  def Disk(height: Double, radius: Double, thetaMax: Double, params: Any*): Unit =
    ctx.value foreach (_.disk(height, radius, thetaMax, extractParams(params: _*)))
  def Torus(majorRadius: Double, minorRadius: Double, phiMin: Double, phiMax: Double, thetaMax: Double, 
    params: PMap): Unit =
    ctx.value foreach (_.torus(majorRadius, minorRadius, phiMin, phiMax, thetaMax, params))
  def Torus(majorRadius: Double, minorRadius: Double, phiMin: Double, phiMax: Double, thetaMax: Double, 
    params: Any*): Unit =
    ctx.value foreach (_.torus(majorRadius, minorRadius, phiMin, phiMax, thetaMax, extractParams(params: _*)))

  // General objects
  def ObjectInstance(handle: ObjectHandle): Unit = ctx.value foreach (_.objectInstance(handle))
  def Procedural(name: String, args: Seq[Any], bound: BoundBox): Unit =
    ctx.value foreach (_.procedural(name, args, bound))
  def DelayedReadArchive(ribFileName: String, bound: BoundBox): Unit =
    Procedural("DelayedReadArchive", Seq("\"%s\"" format(ribFileName)), bound)
  def RunProgram(exeName: String, cmdLineArgs: String, bound: BoundBox): Unit =
    Procedural("RunProgram", Seq("\"%s\"" format(exeName), "\"%s\"" format(cmdLineArgs)), bound)
  def DynamicLoad(dsoName: String, args: String, bound: BoundBox): Unit =
    Procedural("DynamicLoad", Seq("\"%s\"" format(dsoName), "\"%s\"" format(args)), bound)
  def Geometry(name: String, params: PMap): Unit = ctx.value foreach (_.geometry(name, params))
  def Geometry(name: String, params: Any*): Unit = ctx.value foreach (_.geometry(name, extractParams(params: _*)))

  // Map-making
  def MakeTexture(pictureName: String, textureName: String, sWrap: String, tWrap: String, filter: FilterFunc,
    sWidth: Double, tWidth: Double, params: PMap): Unit =
    ctx.value foreach (_.makeTexture(pictureName, textureName, sWrap, tWrap, filter, sWidth, tWidth, params))
  def MakeTexture(pictureName: String, textureName: String, sWrap: String, tWrap: String, filter: FilterFunc,
    sWidth: Double, tWidth: Double, params: Any*): Unit =
    ctx.value foreach (_.makeTexture(pictureName, textureName, sWrap, tWrap, filter, sWidth, tWidth, 
      extractParams(params: _*)))
  def MakeLatLongEnvironment(pictureName: String, textureName: String, filter: FilterFunc, 
    sWidth: Double, tWidth: Double, params: PMap): Unit =
    ctx.value foreach (_.makeLatLongEnvironment(pictureName, textureName, filter, sWidth, tWidth, params))
  def MakeLatLongEnvironment(pictureName: String, textureName: String, filter: FilterFunc, 
    sWidth: Double, tWidth: Double, params: Any*): Unit =
    ctx.value foreach (_.makeLatLongEnvironment(pictureName, textureName, filter, sWidth, tWidth, 
      extractParams(params: _*)))
  def MakeCubeFaceEnvironment(px: String, nx: String, py: String, ny: String, pz: String, nz: String,
    textureName: String, fov: Double, filter: FilterFunc, sWidth: Double, tWidth: Double, params: PMap): Unit =
    ctx.value foreach (_.makeCubeFaceEnvironment(
      px, nx, py, ny, pz, nz, textureName, fov, filter, sWidth, tWidth, params
    ))
  def MakeCubeFaceEnvironment(px: String, nx: String, py: String, ny: String, pz: String, nz: String,
    textureName: String, fov: Double, filter: FilterFunc, sWidth: Double, tWidth: Double, params: Any*): Unit =
    ctx.value foreach (_.makeCubeFaceEnvironment(
      px, nx, py, ny, pz, nz, textureName, fov, filter, sWidth, tWidth, extractParams(params: _*)
    ))  
  def MakeShadow(pictureName: String, textureName: String, params: PMap): Unit =
    ctx.value foreach (_.makeShadow(pictureName, textureName, params))
  def MakeShadow(pictureName: String, textureName: String, params: Any*): Unit =
    ctx.value foreach (_.makeShadow(pictureName, textureName, extractParams(params: _*)))
}
