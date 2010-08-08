package ri

import java.io.{File, FileWriter, Writer}
import scala.collection.mutable.StringBuilder
import Context._

/** Writer for RIB files. */
trait RibWriter {
  def incrementIndent(): Unit
  def decrementIndent(): Unit
  def writeln(str: String): Unit
  def close(): Unit
}

/** A (potentially) useful base class for RibWriters. */
abstract class RibWriterBase extends RibWriter {
  private var indent: Int = 0
  protected var indentStr: String = ""
  def incrementIndent(): Unit = { 
    indent += 1
    indentStr = "  " * indent
  }
  def decrementIndent(): Unit = { 
    indent -= 1
    indentStr = "  " * indent
  }
  def close(): Unit = { }
}

/** Default RIB file writer that writes directly to the command line. */
class DefaultRibWriter extends RibWriterBase {
  def writeln(str: String): Unit = println("%s%s" format(indentStr, str))
}

/** A RIB file writer that uses a normal IO Writer. */
class WriterRibWriter(writer: Writer) extends RibWriterBase {
  def writeln(str: String): Unit = writer.write("%s%s\n" format(indentStr, str))
  override def close(): Unit = writer.close()
}

/** A RIB file writer that writes to files. */
class FileRibWriter(file: File) extends WriterRibWriter(new FileWriter(file))
object FileRibWriter {
  def apply(file: File): FileRibWriter = new FileRibWriter(file)
  def apply(fileName: String): FileRibWriter = new FileRibWriter(new File(fileName))
}

case class RibObjectHandle(id: Int) extends ObjectHandle
case class RibLightHandle(id: Int) extends LightHandle

trait RibBlockCreator {
  self: RibContext =>
  protected def startBlock(name: String, suffix: String = ""): Unit = {
    wln("%sBegin %s" format(name, suffix))
    incrementIndent()
  }
  protected def endBlock(name: String): Unit = {
    decrementIndent()
    wln("%sEnd" format(name))
  }
}

trait RibFrameCreator extends RibBlockCreator {
  self: RibContext =>
  override def frameBegin(frame: Int) = startBlock("Frame", frame.toString)
  override def frameEnd() = endBlock("Frame")
  override def getFrameContext(): RibFrameContext = new RibFrameContext(writer, handles)
}

trait RibWorldCreator extends RibBlockCreator {
  self: RibContext =>
  override def worldBegin() = startBlock("World")
  override def worldEnd() = endBlock("World")
  override def getWorldContext(): RibWorldContext = new RibWorldContext(writer, handles)
}

trait RibAttributeCreator extends RibBlockCreator {
  self: RibContext =>
  override def attributeBegin() = startBlock("Attribute")
  override def attributeEnd() = endBlock("Attribute")
  override def getAttributeContext(): RibAttributeContext = new RibAttributeContext(writer, handles)
}

trait RibTransformCreator extends RibBlockCreator {
  self: RibContext =>
  override def transformBegin() = startBlock("Transform")
  override def transformEnd() = endBlock("Transform")
  override def getTransformContext(): RibTransformContext = new RibTransformContext(writer, handles)
}

trait RibSolidCreator extends RibBlockCreator {
  self: RibContext =>
  override def solidBegin(op: SolidOp) = startBlock("Solid", "\"%s\"" format(op.name))
  override def solidEnd() = endBlock("Solid")
  override def getSolidContext(): RibSolidContext = new RibSolidContext(writer, handles)
}

trait RibObjectCreator extends RibBlockCreator {
  self: RibContext =>
  override def objectBegin() = startBlock("Object", "%s" format(nextObjectNumber()))
  override def objectEnd() = endBlock("Object")
  override def getObjectContext(): RibObjectContext = new RibObjectContext(writer, handles)
}

trait RibMotionCreator extends RibBlockCreator {
  self: RibContext =>
  override def motionBegin(times: Seq[Double]) = startBlock("Motion", "%s" format(seqToRib(times)))
  override def motionEnd() = endBlock("Motion")
  override def getMotionContext(): RibMotionContext = new RibMotionContext(writer, handles)
}

trait RibDeclare {
  self: RibContext =>
  override def declare(name: String, declaration: String): Unit = wln("Declare \"%s\" \"%s\"" 
    format(name, declaration))
}

trait RibOptions {
  self: RibContext =>
  override def format(xres: Int, yres: Int, aspect: Double): Unit = wln("Format %s %s %s" format(xres, yres, aspect))
  override def frameAspectRatio(aspect: Double): Unit = wln("FrameAspectRatio %s" format(aspect))
  override def screenWindow(left: Double, right: Double, bottom: Double, top: Double): Unit =
    wln("ScreenWindow %s" format(seqToRib(Seq(left, right, bottom, top))))
  override def cropWindow(xmin: Double, xmax: Double, ymin: Double, ymax: Double): Unit =
    wln("CropWindow %s" format(seqToRib(Seq(xmin, xmax, ymin, ymax))))
  override def projection(pType: ProjectionType, params: PMap): Unit = 
    wln("Projection \"%s\"%s" format(pType.name, pMapToRib(params)))
  override def clipping(near: Double, far: Double): Unit = wln("Clipping %s %s" format(near, far))
  override def depthOfField(fStop: Double, focalLength: Double, focalDistance: Double): Unit = 
    wln("DepthOfField %s %s %s" format(fStop, focalLength, focalDistance))
  override def shutter(min: Double, max: Double): Unit = wln("Shutter %s %s" format(min, max))
  override def pixelVariance(variation: Double): Unit = wln("PixelVariance %s" format(variation))
  override def pixelSamples(xsamples: Double, ysamples: Double): Unit = 
    wln("PixelSamples %s %s" format(xsamples, ysamples))
  override def pixelFilter(filterFunc: FilterFunc, xwidth: Double, ywidth: Double): Unit =
    wln("PixelFilter \"%s\" %s %s" format(filterFunc.name, xwidth, ywidth))
  override def exposure(gain: Double, gamma: Double): Unit = wln("Exposure %s %s" format(gain, gamma))
  override def imager(name: String, params: PMap): Unit = wln("Imager \"%s\"%s" format(name, pMapToRib(params)))
  override def quantize(qType: QuantizeType, one: Int, min: Int, max: Int, ditherAmplitude: Double): Unit =
    wln("Quantize \"%s\" %s %s %s %s" format(qType.name, one, min, max, ditherAmplitude))
  override def display(name: String, dType: DisplayType, dMode: DisplayMode, params: PMap): Unit =
    wln("Display \"%s\" \"%s\" \"%s\"%s" format(name, dType.name, dMode.name, pMapToRib(params)))
  override def hider(name: String, params: PMap): Unit = wln("Hider \"%s\"%s" format(name, pMapToRib(params)))
  override def colorSamples(nRGB: Seq[Double], RGBn: Seq[Double]): Unit =
    wln("ColorSamples %s %s" format(seqToRib(nRGB), seqToRib(RGBn)))
  override def relativeDetail(relativeDetail: Double): Unit = wln("RelativeDetail %s" format(relativeDetail))
  override def option(name: String, params: PMap): Unit = wln("Option \"%s\"%s" format(name, pMapToRib(params)))
}

trait RibLights {
  self: RibContext =>
  override def lightSource(shaderName: String, params: PMap): LightHandle = {
    val handle = RibLightHandle(nextLightNumber())
    wln("LightSource \"%s\" %s%s" format(shaderName, handle.id, pMapToRib(params)))
    handle
  }
  override def areaLightSource(shaderName: String, params: PMap): LightHandle = {
    val handle = RibLightHandle(nextLightNumber())
    wln("AreaLightSource \"%s\" %s%s" format(shaderName, handle.id, pMapToRib(params)))
    handle
  }
}

trait RibAttributes {
  self: RibContext =>
  override def attribute(name: String, params: PMap): Unit = wln("Attribute \"%s\"%s" format(name, pMapToRib(params)))
  override def color(c: Seq[Double]): Unit = wln("Color %s" format(seqToRib(c)))
  override def opacity(c: Seq[Double]): Unit = wln("Opacity %s" format(seqToRib(c)))
  override def surface(shaderName: String, params: PMap): Unit = 
    wln("Surface \"%s\"%s" format(shaderName, pMapToRib(params)))
  override def atmosphere(shaderName: String, params: PMap): Unit =
    wln("Atmosphere \"%s\"%s" format(shaderName, pMapToRib(params)))
  override def interior(shaderName: String, params: PMap): Unit =
    wln("Interior \"%s\"%s" format(shaderName, pMapToRib(params)))
  override def exterior(shaderName: String, params: PMap): Unit =
    wln("Exterior \"%s\"%s" format(shaderName, pMapToRib(params)))
  override def illuminate(handle: LightHandle, onOff: Boolean): Unit = {
    val ribHandle: RibLightHandle = handle.asInstanceOf[RibLightHandle]
    wln("Illuminate %s %s" format(ribHandle.id, boolToRib(onOff)))
  }
  override def displacement(shaderName: String, params: PMap): Unit = 
    wln("Displacement \"%s\"%s" format(shaderName, pMapToRib(params)))
  override def textureCoordinates(s1: Double, t1: Double, s2: Double, t2: Double, 
    s3: Double, t3: Double, s4: Double, t4: Double): Unit =
    wln("TextureCoordinates [ %s %s %s %s %s %s %s %s ]" format(s1, t1, s2, t2, s3, t3, s4, t4))
  override def shadingRate(size: Double): Unit = wln("ShadingRate %s" format(size))
  override def shadingInterpolation(iType: ShadingInterpolationType): Unit = 
    wln("ShadingInterpolation \"%s\"" format(iType.name))
  override def matte(onOff: Boolean): Unit = wln("Matte %s" format(boolToRib(onOff)))
  override def bound(bound: BoundBox): Unit = wln("Bound %s" format(boundToRib(bound)))
  override def detail(bound: BoundBox): Unit = wln("Detail %s" format(boundToRib(bound)))
  override def detailRange(minVisible: Double, lowerTransition: Double, upperTransition: Double, 
    maxVisible: Double): Unit =
    wln("DetailRange [ %s %s %s %s ]" format(minVisible, lowerTransition, upperTransition, maxVisible))
  override def geometricApproximation(aType: String, value: Double): Unit =
    wln("GeometricApproximation \"%s\" %s" format(aType, value))
  override def orientation(orientation: OrientationType): Unit = wln("Orientation \"%s\"" format(orientation.name))
  override def reverseOrientation(): Unit = wln("ReverseOrientation")
  override def sides(sides: Int): Unit = {
    require((sides == 1) || (sides == 2))
    wln("Sides %s" format(sides))
  }
  override def basis(uBasis: Basis, uStep: Int, vBasis: Basis, vStep: Int): Unit =
    wln("Basis %s %s %s %s" format(basisToRib(uBasis), uStep, basisToRib(vBasis), vStep))
  override def trimCurve(nCurves: Seq[Int], order: Seq[Int], knot: Seq[Double], min: Double, max: Double,
    n: Seq[Int], u: Seq[Double], v: Seq[Double], w: Seq[Double]): Unit =
    wln("TrimCurve %s %s %s %s %s %s %s %s %s" format(
      seqToRib(nCurves), seqToRib(order), seqToRib(knot), min, max, seqToRib(n), seqToRib(u), seqToRib(v), seqToRib(w)
    ))
}

trait RibTransformations {
  self: RibContext =>
  override def identity() = wln("Identity")
  override def transform(transform: Matrix): Unit = wln("Transform %s" format(matrixToRib(transform)))
  override def concatTransform(transform: Matrix): Unit = wln("ConcatTransform %s" format(matrixToRib(transform)))
  override def perspective(fov: Double): Unit = wln("Perspective %s" format(fov))
  override def translate(dx: Double, dy: Double, dz: Double): Unit = wln("Translate %s %s %s" format (dx, dy, dz))
  override def rotate(angle: Double, dx: Double, dy: Double, dz: Double): Unit = 
    wln("Rotate %s %s %s %s" format (angle, dx, dy, dz))
  override def scale(sx: Double, sy: Double, sz: Double): Unit = wln("Scale %s %s %s" format (sx, sy, sz))
  override def skew(
    angle: Double, 
    dx1: Double, dy1: Double, dz1: Double, 
    dx2: Double, dy2: Double, dz2: Double
  ): Unit = wln("Skew %s" format(seqToRib(Seq(angle, dx1, dy1, dz1, dx2, dy2, dz2))))
}

trait RibPolygons {
  self: RibContext =>
  override def polygon(params: PMap): Unit = wln("Polygon%s" format(pMapToRib(params)))
  override def generalPolygon(nVertices: Seq[Int], params: PMap): Unit =
    wln("GeneralPolygon %s%s" format(seqToRib(nVertices), pMapToRib(params)))
  override def pointsPolygons(nVertices: Seq[Int], vertices: Seq[Int], params: PMap): Unit =
    wln("PointsPolygons %s %s%s" format(seqToRib(nVertices), seqToRib(vertices), pMapToRib(params)))
  override def pointsGeneralPolygons(nLoops: Seq[Int], nVertices: Seq[Int], vertices: Seq[Int], params: PMap): Unit =
    wln("PointsGeneralPolygons %s %s %s%s" format(seqToRib(nLoops), seqToRib(nVertices), seqToRib(vertices),
      pMapToRib(params)))
}

trait RibPatches {
  self: RibContext =>
  override def patch(pType: PatchType, params: PMap): Unit = 
    wln("Patch \"%s\"%s" format(pType.name, pMapToRib(params)))
  override def patchMesh(pType: PatchType, nu: Int, uWrap: PatchWrap, nv: Int, vWrap: PatchWrap, params: PMap): Unit =
    wln("PatchMesh \"%s\" %s \"%s\" %s \"%s\"%s" format(pType.name, nu, uWrap.name, nv, vWrap.name, pMapToRib(params)))
  override def nuPatch(nu: Int, uOrder: Int, uKnot: Seq[Double], uMin: Double, uMax: Double,
    nv: Int, vOrder: Int, vKnot: Seq[Double], vMin: Double, vMax: Double, params: PMap): Unit =
    wln("NuPatch %s %s %s %s %s %s %s %s %s %s%s" format(
      nu, uOrder, seqToRib(uKnot), uMin, uMax, nv, vOrder, seqToRib(vKnot), vMin, vMax, pMapToRib(params)
    ))
}

trait RibQuadrics {
  self: RibContext =>
  override def sphere(radius: Double, zmin: Double, zmax: Double, thetaMax: Double, params: PMap): Unit = 
    wln("Sphere [ %s %s %s %s ]%s" format(radius, zmin, zmax, thetaMax, pMapToRib(params)))
  override def cone(height: Double, radius: Double, thetaMax: Double, params: PMap): Unit =
    wln("Cone [ %s %s %s ]%s" format(height, radius, thetaMax, pMapToRib(params)))
  override def cylinder(radius: Double, zmin: Double, zmax: Double, thetaMax: Double, params: PMap): Unit =
    wln("Cylinder [ %s %s %s %s ]%s" format(radius, zmin, zmax, thetaMax, pMapToRib(params)))
  override def hyperboloid(point1: Seq[Double], point2: Seq[Double], thetaMax: Double, params: PMap): Unit = 
    wln("Hyperboloid [ %s %s %s ]%s" format(point1.mkString(" "), point2.mkString(" "), thetaMax, pMapToRib(params)))
  override def paraboloid(rmax: Double, zmin: Double, zmax: Double, thetaMax: Double, params: PMap): Unit =
    wln("Paraboloid [ %s %s %s %s ]%s" format(rmax, zmin, zmax, thetaMax, pMapToRib(params)))
  override def disk(height: Double, radius: Double, thetaMax: Double, params: PMap): Unit =
    wln("Disk [ %s %s %s ]%s" format(height, radius, thetaMax, pMapToRib(params)))
  override def torus(majorRadius: Double, minorRadius: Double, phiMin: Double, phiMax: Double, thetaMax: Double, 
    params: PMap): Unit =
    wln("Torus [ %s %s %s %s %s ]%s" format(majorRadius, minorRadius, phiMin, phiMax, thetaMax, pMapToRib(params)))
}

trait RibGeometry extends RibPolygons with RibPatches with RibQuadrics {
  self: RibContext =>
}

trait RibObjectInstance {
  self: RibContext =>
  override def objectInstance(handle: ObjectHandle): Unit = {
    val id = (handle.asInstanceOf[RibObjectHandle]).id
    wln("ObjectInstance %s" format(id))
  }
}

trait RibGeneralObjects {
  self: RibContext =>
  override def procedural(name: String, args: Seq[Any], bound: BoundBox): Unit =
    wln("Procedural \"%s\" %s %s" format(name, seqToRib(args), boundToRib(bound)))
  override def geometry(name: String, params: PMap): Unit = wln("Geometry \"%s\"%s" format(name, pMapToRib(params)))
}

trait RibMapMaking {
  self: RibContext =>
  override def makeTexture(pictureName: String, textureName: String, sWrap: String, tWrap: String,
    filter: FilterFunc, sWidth: Double, tWidth: Double, params: PMap): Unit =
    wln("MakeTexture \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" %s %s%s" format(
      pictureName, textureName, sWrap, tWrap, filter.name, sWidth, tWidth, pMapToRib(params)
    ))
  override def makeLatLongEnvironment(pictureName: String, textureName: String, filter: FilterFunc,
    sWidth: Double, tWidth: Double, params: PMap): Unit =
    wln("MakeLatLongEnvironment \"%s\" \"%s\" \"%s\" %s %s%s" format(
      pictureName, textureName, filter.name, sWidth, tWidth, pMapToRib(params)
    ))
  override def makeCubeFaceEnvironment(px: String, nx: String, py: String, ny: String, pz: String, nz: String,
    textureName: String, fov: Double, filter: FilterFunc, sWidth: Double, tWidth: Double, params: PMap): Unit =
    wln("MakeCubeFaceEnvironment \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" %s \"%s\" %s %s%s" format(
      px, nx, py, ny, pz, nz, textureName, fov, filter.name, sWidth, tWidth, pMapToRib(params)
    ))
  override def makeShadow(pictureName: String, textureName: String, params: PMap): Unit =
    wln("MakeShadow \"%s\" \"%s\"%s" format(pictureName, textureName, pMapToRib(params)))
}


class RibTopContext(writer: RibWriter) extends RibContext(writer, new ContextHandles)
  with RibDeclare
  with RibFrameCreator
  with RibWorldCreator
  with RibAttributeCreator
  with RibTransformCreator
  with RibObjectCreator
  with RibMotionCreator
  with RibOptions
  with RibLights
  with RibAttributes
  with RibTransformations
  with RibMapMaking
{
  wln("version 3.03")
}

class RibFrameContext(writer: RibWriter, ch: ContextHandles) extends RibContext(writer, ch)
  with RibDeclare
  with RibWorldCreator
  with RibAttributeCreator
  with RibTransformCreator
  with RibObjectCreator
  with RibMotionCreator
  with RibOptions
  with RibLights
  with RibAttributes
  with RibTransformations
  with RibMapMaking
{
  
}

class RibWorldContext(writer: RibWriter, ch: ContextHandles) extends RibContext(writer, ch)
  with RibDeclare
  with RibAttributeCreator
  with RibTransformCreator
  with RibSolidCreator
  with RibObjectCreator
  with RibMotionCreator
  with RibLights
  with RibAttributes
  with RibTransformations
  with RibGeometry
  with RibObjectInstance
  with RibGeneralObjects
{
  
}

class RibAttributeContext(writer: RibWriter, ch: ContextHandles) extends RibContext(writer, ch)
  with RibDeclare
  with RibAttributeCreator
  with RibTransformCreator
  with RibSolidCreator
  with RibObjectCreator
  with RibMotionCreator
  with RibLights
  with RibAttributes
  with RibTransformations
  with RibGeometry
  with RibObjectInstance
  with RibGeneralObjects  
{
  
}

class RibTransformContext(writer: RibWriter, ch: ContextHandles) extends RibContext(writer, ch)
  with RibDeclare
  with RibAttributeCreator
  with RibTransformCreator
  with RibSolidCreator
  with RibObjectCreator
  with RibMotionCreator
  with RibLights
  with RibAttributes
  with RibTransformations
  with RibGeometry
  with RibObjectInstance
  with RibGeneralObjects  
{
  
}

class RibSolidContext(writer: RibWriter, ch: ContextHandles) extends RibContext(writer, ch)
  with RibDeclare
  with RibAttributeCreator
  with RibTransformCreator
  with RibSolidCreator
  with RibObjectCreator
  with RibMotionCreator
  with RibLights
  with RibAttributes
  with RibTransformations
  with RibGeometry
  with RibObjectInstance
  with RibGeneralObjects
{
  
}

class RibObjectContext(writer: RibWriter, ch: ContextHandles) extends RibContext(writer, ch)
  with RibDeclare
  with RibGeometry
  with RibGeneralObjects
{
  override def getObjectHandle(): ObjectHandle = new RibObjectHandle(currentObjectNumber())
}

class RibMotionContext(writer: RibWriter, ch: ContextHandles) extends RibContext(writer, ch)
  with RibDeclare
  with RibLights
  with RibAttributes
  with RibTransformations
  with RibGeometry  
{
  
}


/* Tracks light and object handles within Contexts. */
protected class ContextHandles {
  /** Tracks Ints used for handle identifiers. */
  private class IntegerHandleTracker {
    private var handle: Int = 0
    def currentHandle(): Int = handle
    def nextHandle(): Int = {
      handle += 1
      handle
    }
  }

  private val lightTracker = new IntegerHandleTracker
  private val objectTracker = new IntegerHandleTracker
  def nextLightNumber(): Int = lightTracker.nextHandle()
  def nextObjectNumber(): Int = objectTracker.nextHandle()
  def currentObjectNumber(): Int = objectTracker.currentHandle()
}


/** Context for RIB file writing. */
class RibContext(val writer: RibWriter, val handles: ContextHandles) extends Context {
  def close(): Unit = writer.close()
  protected def incrementIndent(): Unit = writer.incrementIndent()
  protected def decrementIndent(): Unit = writer.decrementIndent()
  protected def wln(str: String): Unit = writer.writeln(str)
  protected def seqToRib[A](seq: Seq[A]): String = seq.mkString("[ ", " ", " ]")
  protected def matrixToRib(m: Matrix): String = seqToRib(Seq(
    m.e11, m.e12, m.e13, m.e14,
    m.e21, m.e22, m.e23, m.e24,
    m.e31, m.e32, m.e33, m.e34,
    m.e41, m.e42, m.e43, m.e44
  ))
  protected def basisToRib(b: Basis): String = {
    if (b.name.isDefined) {
      "\"%s\"" format b.name.get
    } else {
      require(b.matrix.isDefined)
      matrixToRib(b.matrix.get)
    }
  }
  protected def boundToRib(b: BoundBox): String = seqToRib(Seq(
    b.xmin, b.xmax, b.ymin, b.ymax, b.zmin, b.zmax
  ))
  protected def pMapToRib(params: PMap): String = {
    val sb = new StringBuilder()
    for ((key, value) <- params) {
      sb.append(" \"")
      sb.append(key)
      sb.append("\" ")
      sb.append(seqToRib(value))
    }
    sb.toString
  }
  protected def boolToRib(value: Boolean): String = Map[Boolean, String](true->"1", false->"0")(value)
  
  def nextLightNumber(): Int = handles.nextLightNumber()
  def nextObjectNumber(): Int = handles.nextObjectNumber()
  def currentObjectNumber(): Int = handles.currentObjectNumber()
}
