
import org.apache.commons.codec.binary.Base64;
 
import java.io.*;
 
public class String2File {
    public static void main(String[] args) {
 
        String string = "SGVsbG8gV29ybGQNCkhlbGxvIEphdmENCi4uLg==";
        String filePath = "d:\\test1.txt";
        stirng2File(string, filePath);
 
    }
 
    public static void stirng2File(String base64Code, String targetPath) {
        System.out.println("20个字符" + base64Code.substring(0, 50));
        System.out.println("BASE64转译String333:" + base64Code.length());
        byte[] buffer;
        FileOutputStream out = null;
        FileOutputStream out2 = null;
        try {
            Base64 base64 = new Base64();
            //解码
            buffer = base64.decode(base64Code);
            //buffer = new BASE64Decoder().decodeBuffer(base64Code);
            System.out.println("BASE64转译byte111:" + buffer.length);
            //System.out.println("数据为"+temp);
            //System.out.println("buffer长度"+buffer.length);
            out = new FileOutputStream(targetPath);
//            out2 = new FileOutputStream("D:\\test\\1.txt");
            out.write(buffer);
//            out2.write(buffer);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } finally {
            if (out != null) {
                try {
                    out.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
            if (out2 != null) {
                try {
                    out2.close();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
 
        }
 
    }
}