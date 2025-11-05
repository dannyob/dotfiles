#!/usr/bin/env swift

import Cocoa
import Foundation

// 1. Get the general pasteboard
let pasteboard = NSPasteboard.general

// 2. Try to read an NSImage from the clipboard
guard let image = NSImage(pasteboard: pasteboard) else {
    fputs("No image found on the clipboard.\n", stderr)
    exit(1)
}

// 3. Convert the image to PNG data
guard
    let tiffData = image.tiffRepresentation,
    let bitmap = NSBitmapImageRep(data: tiffData),
    let pngData = bitmap.representation(using: .png, properties: [:])
else {
    fputs("Failed to convert image to PNG.\n", stderr)
    exit(1)
}

// 4. Write the PNG data to a temporary location
let tempDir = URL(fileURLWithPath: NSTemporaryDirectory())
let tempFileURL = tempDir.appendingPathComponent("clipboard_image.png")
do {
    try pngData.write(to: tempFileURL, options: .atomic)
} catch {
    fputs("Failed to write PNG to disk: \(error)\n", stderr)
    exit(1)
}

// 5. Use AppleScript to tell Photos to import that file
let scriptSource = """
    tell application "Photos"
        import POSIX file "\(tempFileURL.path)"
    end tell
    """

if let script = NSAppleScript(source: scriptSource) {
    var errorDict: NSDictionary?
    script.executeAndReturnError(&errorDict)

    if let err = errorDict {
        fputs("AppleScript error: \(err)\n", stderr)
        exit(1)
    } else {
        print("Successfully imported image into Photos.")
    }
} else {
    fputs("Failed to create AppleScript object.\n", stderr)
    exit(1)
}
